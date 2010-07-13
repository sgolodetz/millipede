/***
 * millipede: CubeInternalGenerator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBEINTERNALGENERATOR
#define H_MILLIPEDE_CUBEINTERNALGENERATOR

#include <common/io/util/OSSWrapper.h>
#include <common/jobs/SimpleJob.h>
#include "MeshBuildingData.h"

namespace mp {

/**
@brief	A CubeInternalGenerator handles node and edge generation within a cube.

The nodes and edges added depend on the number of face-centre nodes.

-	If there are 0 face centre nodes, nothing needs doing.
-	If there are 2 face-centre nodes, an edge needs to be added between them.
-	If there are > 2 face-centre nodes, a cube centre node needs to be added, along with edges joining it to the face-centre nodes.

It is not possible for there to be a single face-centre node, so that case is explicitly excluded.

@tparam	Label	The type of label stored at the cube vertices and in the mesh nodes
*/
template <typename Label>
class CubeInternalGenerator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef GlobalNodeTable<Label> GlobalNodeTableT;
	typedef MeshBuildingData<Label> MeshBuildingDataT;
	typedef boost::shared_ptr<MeshBuildingDataT> MeshBuildingData_Ptr;
	typedef MeshNode<Label> MeshNodeT;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshBuildingData_Ptr m_data;
	int m_x, m_y, m_z;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Constructs a CubeInternalGenerator job to work on the specified cube.

	@param[in]	data			The mesh building data shared by all sub-jobs of MeshBuilder
	@param[in]	x				The x position of the cube in the volume
	@param[in]	y				The y position of the cube in the volume
	@param[in]	z				The z position of the cube in the volume
	*/
	CubeInternalGenerator(const MeshBuildingData_Ptr& data, int x, int y, int z)
	:	m_data(data), m_x(x), m_y(y), m_z(z)
	{}

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Returns the length of the job.

	@return	As described
	*/
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	/**
	@brief	Executes the job.
	*/
	void execute_impl()
	{
		set_status(OSSWrapper() << "Generating internals of cube (" << m_x << ',' << m_y << ',' << m_z << ')');

		CubeTable& cubeTable = m_data->cube_table();
		GlobalNodeTableT& globalNodeTable = m_data->global_node_table();

		std::vector<int> faceCentreNodes = cubeTable.lookup_face_centre_nodes(m_x, m_y, m_z);

		// Add additional nodes and edges as necessary depending on the number of face-centre nodes.
		switch(faceCentreNodes.size())
		{
			case 0:
			{
				break;
			}
			case 2:
			{
				// Add an edge joining the two face-centre nodes.
				MeshNodeT& fc0 = globalNodeTable(faceCentreNodes[0]);
				MeshNodeT& fc1 = globalNodeTable(faceCentreNodes[1]);
				fc0.add_adjacent_node(faceCentreNodes[1]);
				fc1.add_adjacent_node(faceCentreNodes[0]);
				break;
			}
			default:	// > 2 face-centre nodes
			{
				// Create a cube-centre node, assign it all the labels of the cube, and join it to all the face centres.
				int cubeCentreNode = globalNodeTable.find_index(typename GlobalNodeTableT::NodePosition(Vector3i(m_x, m_y, m_z), GlobalNodeTableT::OFFSET_111));
				cubeTable.set_cube_centre_node(m_x, m_y, m_z, cubeCentreNode);

				MeshNodeT& c = globalNodeTable(cubeCentreNode);
				std::vector<Vector3i> vertexPositions = cubeTable.cube_vertices(m_x, m_y, m_z);
				for(int i=0; i<8; ++i)
				{
					Label vertexLabel = m_data->label(vertexPositions[i]);
					c.add_sourced_label(vertexLabel, vertexPositions[i]);
				}

				for(size_t i=0, size=faceCentreNodes.size(); i<size; ++i)
				{
					MeshNodeT& fc = globalNodeTable(faceCentreNodes[i]);
					c.add_adjacent_node(faceCentreNodes[i]);
					fc.add_adjacent_node(cubeCentreNode);
				}
				break;
			}
		}
	}
};

}

#endif
