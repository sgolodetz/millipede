/***
 * millipede: MeshBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHBUILDER
#define H_MILLIPEDE_MESHBUILDER

#include <functional>

#include <common/jobs/CompositeJob.h>
#include "CubeFaceGenerator.h"
#include "CubeInternalGenerator.h"
#include "CubeTriangleGenerator.h"
#include "Mesh.h"

namespace mp {

/**
@brief	A MeshBuilder builds a 3D mesh from a 3D label image using the multiple material marching cubes (M3C) algorithm.

@tparam	Label			The type of label to be used
@tparam	PriorityPred	A predicate type defining an ordering over the labels for resolving conflicts that arise during the algorithm
*/
template <typename Label, typename PriorityPred = std::less<Label> >
class MeshBuilder : public CompositeJob
{
	//#################### TYPEDEFS ####################
public:
	typedef itk::Image<Label,3> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;
private:
	typedef GlobalNodeTable<Label> GlobalNodeTableT;
	typedef Mesh<Label> MeshT;
	typedef boost::shared_ptr<MeshT> Mesh_Ptr;
	typedef MeshBuildingData<Label> MeshBuildingDataT;
	typedef boost::shared_ptr<MeshBuildingDataT> MeshBuildingData_Ptr;
	typedef MeshNode<Label> MeshNodeT;
	typedef MeshTriangle<Label> MeshTriangleT;
	typedef std::list<MeshTriangleT> MeshTriangleList;

	//#################### JOBS ####################
private:
	template <typename Spawnee>
	struct ForEachCubeFaceJobSpawner : SimpleJob
	{
		MeshBuildingData_Ptr data;
		int xSize, ySize, zSize;
		int xDim[3], yDim[3], zDim[3];

		mutable boost::mutex mut;
		boost::shared_ptr<Spawnee> spawnee;
		int spawneeCount;

		ForEachCubeFaceJobSpawner(const MeshBuildingData_Ptr& data_, int xSize_, int ySize_, int zSize_)
		:	data(data_), xSize(xSize_), ySize(ySize_), zSize(zSize_)
		{
			xDim[CubeFaceDesignator::FACE_XY] = xSize;		xDim[CubeFaceDesignator::FACE_XZ] = xSize;		xDim[CubeFaceDesignator::FACE_YZ] = xSize+1;
			yDim[CubeFaceDesignator::FACE_XY] = ySize;		yDim[CubeFaceDesignator::FACE_XZ] = ySize+1;	yDim[CubeFaceDesignator::FACE_YZ] = ySize;
			zDim[CubeFaceDesignator::FACE_XY] = zSize+1;	zDim[CubeFaceDesignator::FACE_XZ] = zSize;		zDim[CubeFaceDesignator::FACE_YZ] = zSize;

			spawneeCount = 0;
			for(CubeFaceDesignator::Enum f=enum_begin<CubeFaceDesignator::Enum>(), end=enum_end<CubeFaceDesignator::Enum>(); f!=end; ++f)
			{
				spawneeCount += xDim[f] * yDim[f] * zDim[f];
			}
		}

		void execute_impl()
		{
			for(CubeFaceDesignator::Enum f=enum_begin<CubeFaceDesignator::Enum>(), end=enum_end<CubeFaceDesignator::Enum>(); f!=end; ++f)
				for(int x=0; x<xDim[f]; ++x)
					for(int y=0; y<yDim[f]; ++y)
						for(int z=0; z<zDim[f]; ++z)
						{
							{
								boost::mutex::scoped_lock lock(mut);
								spawnee.reset(new Spawnee(data, x, y, z, f));
							}
							spawnee->execute();

							if(is_aborted()) return;
							increment_progress();
						}
		}

		int length() const
		{
			return spawneeCount;
		}

		std::string status() const
		{
			boost::mutex::scoped_lock lock(mut);
			return spawnee ? spawnee->status() : "";
		}
	};

	template <typename Spawnee>
	struct ForEachCubeJobSpawner : SimpleJob
	{
		MeshBuildingData_Ptr data;
		int xSize, ySize, zSize;

		mutable boost::mutex mut;
		boost::shared_ptr<Spawnee> spawnee;

		ForEachCubeJobSpawner(const MeshBuildingData_Ptr& data_, int xSize_, int ySize_, int zSize_)
		:	data(data_), xSize(xSize_), ySize(ySize_), zSize(zSize_)
		{}

		void execute_impl()
		{
			for(int z=0; z<zSize; ++z)
				for(int y=0; y<ySize; ++y)
					for(int x=0; x<xSize; ++x)
					{
						{
							boost::mutex::scoped_lock lock(mut);
							spawnee.reset(new Spawnee(data, x, y, z));
						}
						spawnee->execute();

						if(is_aborted()) return;
						increment_progress();
					}
		}

		int length() const
		{
			return xSize * ySize * zSize;
		}

		std::string status() const
		{
			boost::mutex::scoped_lock lock(mut);
			return spawnee ? spawnee->status() : "";
		}
	};

	/**
	@brief	The AddTriangleEdgesJob ensures that the adjacent node sets for each node reflect the new edges which have been added during triangulation.
	*/
	struct AddTriangleEdgesJob : SimpleJob
	{
		MeshBuilder *base;

		explicit AddTriangleEdgesJob(MeshBuilder *base_)
		:	base(base_)
		{}

		void execute_impl()
		{
			set_status("Adding triangle edges...");

			GlobalNodeTable<Label>& globalNodeTable = base->m_data->global_node_table();
			const MeshTriangleList& triangles = *base->m_data->triangles();

			for(typename MeshTriangleList::const_iterator it=triangles.begin(), iend=triangles.end(); it!=iend; ++it)
			{
				const MeshTriangleT& tri = *it;
				int i0 = tri.index(0), i1 = tri.index(1), i2 = tri.index(2);
				MeshNodeT& n0 = globalNodeTable(i0);
				MeshNodeT& n1 = globalNodeTable(i1);
				MeshNodeT& n2 = globalNodeTable(i2);
				n0.add_adjacent_node(i1);		n0.add_adjacent_node(i2);
				n1.add_adjacent_node(i0);		n1.add_adjacent_node(i2);
				n2.add_adjacent_node(i0);		n2.add_adjacent_node(i1);
			}
		}

		int length() const
		{
			return 1;
		}
	};

	struct CreateMeshJob : SimpleJob
	{
		MeshBuilder *base;

		explicit CreateMeshJob(MeshBuilder *base_)
		:	base(base_)
		{}

		void execute_impl()
		{
			set_status("Creating mesh...");
			base->m_meshHook.set(Mesh_Ptr(new MeshT(base->m_data->global_node_table().master_array(), base->m_data->triangles())));
		}

		int length() const
		{
			return 1;
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	MeshBuildingData_Ptr m_data;
	DataHook<Mesh_Ptr> m_meshHook;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Constructs the MeshBuilder.

	@note	The volume size must be passed in explicitly here because although the label image may not exist yet,
			its size must be known in order to generate the appropriate mesh-building subjobs.
	@note	If a labelling is not passed in here, it must later be set explicitly using set_labelling().

	@param[in]	volumeSize	The size of the labelled volume
	@param[in]	labelling	An optional itk::SmartPointer to a label image
	@pre
		-	If a labelling is passed in here, it must be non-null
		-	If a labelling is passed in here, its size must equal volumeSize
	*/
	explicit MeshBuilder(const itk::Size<3>& volumeSize, const boost::optional<LabelImagePointer>& labelling = boost::none)
	:	m_data(new MeshBuildingDataT)
	{
		if(labelling) set_labelling(*labelling);

		int xSize = volumeSize[0] - 1, ySize = volumeSize[1] - 1, zSize = volumeSize[2] - 1;

		// Add the spawner for the CubeFaceGenerator sub-jobs.
		add_subjob(new ForEachCubeFaceJobSpawner<CubeFaceGenerator<Label,PriorityPred> >(m_data, xSize, ySize, zSize));

		// Add the spawner for the CubeInternalGenerator sub-jobs.
		add_subjob(new ForEachCubeJobSpawner<CubeInternalGenerator<Label> >(m_data, xSize, ySize, zSize));

		// Add the spawner for the CubeTriangleGenerator sub-jobs.
		add_subjob(new ForEachCubeJobSpawner<CubeTriangleGenerator<Label> >(m_data, xSize, ySize, zSize));

		// Add the AddTriangleEdgesJob sub-job.
		add_subjob(new AddTriangleEdgesJob(this));

		// Add the CreateMeshJob sub-job.
		add_subjob(new CreateMeshJob(this));
	}

	//#################### PUBLIC METHODS ####################
public:
	const Mesh_Ptr& get_mesh() const
	{
		return m_meshHook.get();
	}

	const DataHook<Mesh_Ptr>& get_mesh_hook() const
	{
		return m_meshHook;
	}

	void set_labelling(const LabelImagePointer& labelling)
	{
		m_data->set_labelling(labelling);
	}

	void set_labelling_hook(const DataHook<LabelImagePointer>& labellingHook)
	{
		m_data->set_labelling_hook(labellingHook);
	}
};

}

#endif
