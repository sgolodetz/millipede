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
				fc0.adjacentNodes.insert(faceCentreNodes[1]);
				fc1.adjacentNodes.insert(faceCentreNodes[0]);
				break;
			}
			default:	// > 2 face-centre nodes
			{
				// Create a cube-centre node, assign it all the labels of the cube, and join it to all the face centres.
				int cubeCentreNode = globalNodeTable.find_index(GlobalNodeTableT::NodePosition(Vector3i(m_x, m_y, m_z), GlobalNodeTableT::OFFSET_111));
				cubeTable.set_cube_centre_node(m_x, m_y, m_z, cubeCentreNode);

				MeshNodeT& c = globalNodeTable(cubeCentreNode);
				std::vector<Vector3i> vertexPositions = cubeTable.cube_vertices(m_x, m_y, m_z);
				for(int i=0; i<8; ++i)
				{
					Label vertexLabel = m_data->label(vertexPositions[i]);
					c.sourcedLabels.insert(make_sourced_label(vertexLabel, vertexPositions[i]));
				}

				for(size_t i=0, size=faceCentreNodes.size(); i<size; ++i)
				{
					MeshNodeT& fc = globalNodeTable(faceCentreNodes[i]);
					c.adjacentNodes.insert(faceCentreNodes[i]);
					fc.adjacentNodes.insert(cubeCentreNode);
				}
				break;
			}
		}
	}
};

}

#endif
