/***
 * millipede: CubeInternalGenerator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBEINTERNALGENERATOR
#define H_MILLIPEDE_CUBEINTERNALGENERATOR

#include <common/jobs/SimpleJob.h>
#include "MeshBuildingData.h"

namespace mp {

template <typename Label>
class CubeInternalGenerator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef MeshBuildingData<Label> MeshBuildingDataT;
	typedef boost::shared_ptr<MeshBuildingDataT> MeshBuildingData_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshBuildingData_Ptr m_data;
	int m_x, m_y, m_z;

	//#################### CONSTRUCTORS ####################
public:
	CubeInternalGenerator(const MeshBuildingData_Ptr& data, int x, int y, int z)
	:	m_data(data), m_x(x), m_y(y), m_z(z)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl()
	{
		std::vector<boost::optional<const CubeFace&> > cubeFaces = lookup_cube_faces();

		// Determine the set of nodes used on the cube faces, and calculate the number of face-centre nodes.
		std::set<int> nodeSet;
		std::vector<int> faceCentreNodes;
		for(int i=0; i<6; ++i)
		{
			if(!cubeFaces[i]) continue;

			for(CubeFace::NodeDesignator n=enum_begin<CubeFace::NodeDesignator>(), end=enum_end<CubeFace::NodeDesignator>(); n!=end; ++n)
			{
				if(cubeFaces[i]->is_used(n)) nodeSet.insert(cubeFaces[i]->global_node_index(n));
			}

			if(cubeFaces[i]->is_used(CubeFace::MIDDLE_NODE))
			{
				faceCentreNodes.push_back(cubeFaces[i]->global_node_index(CubeFace::MIDDLE_NODE));
			}
		}

		if(nodeSet.empty()) return;

		// TODO
	}

	/**
	@brief	Looks up the faces of this cube in the cube table.

	@return	A std::vector containing the cube faces (where present)
	*/
	std::vector<boost::optional<const CubeFace&> > lookup_cube_faces() const
	{
		const CubeTable& cubeTable = m_data->cube_table();
		std::vector<boost::optional<const CubeFace&> > cubeFaces(6);
		cubeFaces[0] = cubeTable.lookup_cube_face(m_x,		m_y,	m_z,	CubeFaceDesignator::FACE_XY);
		cubeFaces[1] = cubeTable.lookup_cube_face(m_x,		m_y,	m_z+1,	CubeFaceDesignator::FACE_XY);
		cubeFaces[2] = cubeTable.lookup_cube_face(m_x,		m_y,	m_z,	CubeFaceDesignator::FACE_XZ);
		cubeFaces[3] = cubeTable.lookup_cube_face(m_x,		m_y+1,	m_z,	CubeFaceDesignator::FACE_XZ);
		cubeFaces[4] = cubeTable.lookup_cube_face(m_x,		m_y,	m_z,	CubeFaceDesignator::FACE_YZ);
		cubeFaces[5] = cubeTable.lookup_cube_face(m_x+1,	m_y,	m_z,	CubeFaceDesignator::FACE_YZ);
		return cubeFaces;
	}
};

}

#endif
