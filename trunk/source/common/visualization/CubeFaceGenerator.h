/***
 * millipede: CubeFaceGenerator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBEFACEGENERATOR
#define H_MILLIPEDE_CUBEFACEGENERATOR

#include <set>

#include <common/exceptions/Exception.h>
#include <common/jobs/SimpleJob.h>
#include "MeshBuildingData.h"

namespace mp {

/**
@brief	A CubeFaceGenerator determines the multiple material marching squares (M3S) pattern on a cube face
		and generates global nodes and edges (stored implicitly in the nodes) accordingly. It also builds
		a map from local to global nodes and stores this in the cube face table.

@tparam	Label			The type of label to be used
@tparam	PriorityPred	A predicate type defining an ordering over the labels for resolving conflicts that arise during the algorithm
*/
template <typename Label, typename PriorityPred>
class CubeFaceGenerator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef GlobalNodeTable<Label> GlobalNodeTableT;
	typedef itk::Image<Label,3> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;
	typedef MeshBuildingData<Label> MeshBuildingDataT;
	typedef boost::shared_ptr<MeshBuildingDataT> MeshBuildingData_Ptr;
	typedef MeshNode<Label> MeshNodeT;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshBuildingData_Ptr m_data;
	CubeFaceDesignator::Enum m_faceDesignator;
	int m_x, m_y, m_z;
	LabelImagePointer m_labelling;

	//#################### CONSTRUCTORS ####################
public:
	CubeFaceGenerator(const MeshBuildingData_Ptr& data, int x, int y, int z, CubeFaceDesignator::Enum faceDesignator)
	:	m_data(data), m_faceDesignator(faceDesignator), m_x(x), m_y(y), m_z(z)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	static std::list<CubeFace::Edge> edges_on_face(Label topLeft, Label topRight, Label bottomLeft, Label bottomRight)
	{
		std::list<CubeFace::Edge> edges;

		std::set<Label, PriorityPred> labels;
		labels.insert(topLeft);
		labels.insert(topRight);
		labels.insert(bottomLeft);
		labels.insert(bottomRight);

		const int labelCount = static_cast<int>(labels.size());

		switch(labelCount)
		{
			case 1:
			case 2:
			{
				int a = topRight != topLeft, b = bottomLeft != topLeft, c = bottomRight != topLeft;
				int combo = (a << 2) + (b << 1) + c;
				switch(combo)
				{
					case 0:		// 0000 or 1111
						break;
					case 1:		// 0001 or 1110
						edges.push_back(CubeFace::Edge(CubeFace::RIGHT_NODE, CubeFace::BOTTOM_NODE));
						break;
					case 2:		// 0010 or 1101
						edges.push_back(CubeFace::Edge(CubeFace::LEFT_NODE, CubeFace::BOTTOM_NODE));
						break;
					case 3:		// 0011 or 1100
						edges.push_back(CubeFace::Edge(CubeFace::LEFT_NODE, CubeFace::RIGHT_NODE));
						break;
					case 4:		// 0100 or 1011
						edges.push_back(CubeFace::Edge(CubeFace::RIGHT_NODE, CubeFace::TOP_NODE));
						break;
					case 5:		// 0101 or 1010
						edges.push_back(CubeFace::Edge(CubeFace::TOP_NODE, CubeFace::BOTTOM_NODE));
						break;
					case 6:		// 0110 or 1001
						// Ambiguous case: use label priorities to resolve.
						if(PriorityPred()(topLeft, topRight))	// if the topLeft label has priority over the topRight one
						{
							edges.push_back(CubeFace::Edge(CubeFace::TOP_NODE, CubeFace::RIGHT_NODE));
							edges.push_back(CubeFace::Edge(CubeFace::LEFT_NODE, CubeFace::BOTTOM_NODE));
						}
						else
						{
							edges.push_back(CubeFace::Edge(CubeFace::TOP_NODE, CubeFace::LEFT_NODE));
							edges.push_back(CubeFace::Edge(CubeFace::RIGHT_NODE, CubeFace::BOTTOM_NODE));
						}
						break;
					case 7:		// 0111 or 1000
						edges.push_back(CubeFace::Edge(CubeFace::LEFT_NODE, CubeFace::TOP_NODE));
						break;
				}
				break;
			}
			case 3:
			{
				// Cases: tl=tr, tl=bl, tl=br, tr=bl, tr=br, bl=br
				if(topLeft == topRight)
				{
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(topLeft == bottomLeft)
				{
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(topLeft == bottomRight)
				{
					edges.push_back(CubeFace::Edge(CubeFace::TOP_NODE, CubeFace::RIGHT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::LEFT_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(topRight == bottomLeft)
				{
					edges.push_back(CubeFace::Edge(CubeFace::TOP_NODE, CubeFace::LEFT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::RIGHT_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(topRight == bottomRight)
				{
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				}
				else	// bottomLeft == bottomRight
				{
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
				}
				break;
			}
			default:
			{
				edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
				edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
				edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
				edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				break;
			}
		}

		return edges;
	}

	void execute_impl()
	{
		m_labelling = m_data->labelling();

		// Determine the locations of the face corners.
		Vector3i topLeftLoc, topRightLoc, bottomLeftLoc, bottomRightLoc;
		switch(m_faceDesignator)
		{
			case CubeFaceDesignator::FACE_XY:
				topLeftLoc = Vector3i(m_x,m_y+1,m_z);		topRightLoc = Vector3i(m_x+1,m_y+1,m_z);
				bottomLeftLoc = Vector3i(m_x,m_y,m_z);		bottomRightLoc = Vector3i(m_x+1,m_y,m_z);
				break;
			case CubeFaceDesignator::FACE_XZ:
				topLeftLoc = Vector3i(m_x,m_y,m_z+1);		topRightLoc = Vector3i(m_x+1,m_y,m_z+1);
				bottomLeftLoc = Vector3i(m_x,m_y,m_z);		bottomRightLoc = Vector3i(m_x+1,m_y,m_z);
				break;
			case CubeFaceDesignator::FACE_YZ:
				topLeftLoc = Vector3i(m_x,m_y,m_z+1);		topRightLoc = Vector3i(m_x,m_y+1,m_z+1);
				bottomLeftLoc = Vector3i(m_x,m_y,m_z);		bottomRightLoc = Vector3i(m_x,m_y+1,m_z);
				break;
			default:
				throw Exception("Invalid face designator");		// this should never happen
		}

		// Look up the labels of the face corners.
		Label topLeftLabel = label(topLeftLoc);
		Label topRightLabel = label(topRightLoc);
		Label bottomLeftLabel = label(bottomLeftLoc);
		Label bottomRightLabel = label(bottomRightLoc);

		// Calculate the edges on the face from the labels of the corners.
		std::list<CubeFace::Edge> edges = edges_on_face(topLeftLabel, topRightLabel, bottomLeftLabel, bottomRightLabel);

		// If there aren't any edges, this cube face is irrelevant to the mesh.
		if(edges.size() == 0) return;

		// Construct the cube face, marking each used local node to indicate that we need to look up its global node.
		CubeFace cubeFace;
		for(std::list<CubeFace::Edge>::const_iterator it=edges.begin(), iend=edges.end(); it!=iend; ++it)
		{
			cubeFace.set_used(it->u);
			cubeFace.set_used(it->v);
		}

		// Build the mapping from local node indices to global coordinates.
		Vector3i nodeLocs[CubeFace::POTENTIAL_NODE_COUNT];
		typename GlobalNodeTableT::NodeDesignator nodeDesignators[CubeFace::POTENTIAL_NODE_COUNT];
		switch(m_faceDesignator)
		{
			case CubeFaceDesignator::FACE_XY:
				nodeLocs[CubeFace::TOP_NODE] = Vector3i(m_x,m_y+1,m_z);		nodeDesignators[CubeFace::TOP_NODE] = GlobalNodeTableT::NODE_100;
				nodeLocs[CubeFace::LEFT_NODE] = Vector3i(m_x,m_y,m_z);		nodeDesignators[CubeFace::LEFT_NODE] = GlobalNodeTableT::NODE_010;
				nodeLocs[CubeFace::MIDDLE_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::MIDDLE_NODE] = GlobalNodeTableT::NODE_110;
				nodeLocs[CubeFace::RIGHT_NODE] = Vector3i(m_x+1,m_y,m_z);	nodeDesignators[CubeFace::RIGHT_NODE] = GlobalNodeTableT::NODE_010;
				nodeLocs[CubeFace::BOTTOM_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::BOTTOM_NODE] = GlobalNodeTableT::NODE_100;
				break;
			case CubeFaceDesignator::FACE_XZ:
				nodeLocs[CubeFace::TOP_NODE] = Vector3i(m_x,m_y,m_z+1);		nodeDesignators[CubeFace::TOP_NODE] = GlobalNodeTableT::NODE_100;
				nodeLocs[CubeFace::LEFT_NODE] = Vector3i(m_x,m_y,m_z);		nodeDesignators[CubeFace::LEFT_NODE] = GlobalNodeTableT::NODE_001;
				nodeLocs[CubeFace::MIDDLE_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::MIDDLE_NODE] = GlobalNodeTableT::NODE_101;
				nodeLocs[CubeFace::RIGHT_NODE] = Vector3i(m_x+1,m_y,m_z);	nodeDesignators[CubeFace::RIGHT_NODE] = GlobalNodeTableT::NODE_001;
				nodeLocs[CubeFace::BOTTOM_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::BOTTOM_NODE] = GlobalNodeTableT::NODE_100;
				break;
			case CubeFaceDesignator::FACE_YZ:
				nodeLocs[CubeFace::TOP_NODE] = Vector3i(m_x,m_y,m_z+1);		nodeDesignators[CubeFace::TOP_NODE] = GlobalNodeTableT::NODE_010;
				nodeLocs[CubeFace::LEFT_NODE] = Vector3i(m_x,m_y,m_z);		nodeDesignators[CubeFace::LEFT_NODE] = GlobalNodeTableT::NODE_001;
				nodeLocs[CubeFace::MIDDLE_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::MIDDLE_NODE] = GlobalNodeTableT::NODE_011;
				nodeLocs[CubeFace::RIGHT_NODE] = Vector3i(m_x,m_y+1,m_z);	nodeDesignators[CubeFace::RIGHT_NODE] = GlobalNodeTableT::NODE_001;
				nodeLocs[CubeFace::BOTTOM_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::BOTTOM_NODE] = GlobalNodeTableT::NODE_010;
				break;
			default:
				throw Exception("Invalid face designator");		// this should never happen
		}

		// Lookup the global node indices.
		for(CubeFace::NodeDesignator n=enum_begin<CubeFace::NodeDesignator>(), end=enum_end<CubeFace::NodeDesignator>(); n!=end; ++n)
		{
			if(cubeFace.is_used(n))
			{
				int globalNodeIndex = m_data->global_node_table().find_index(nodeLocs[n], nodeDesignators[n]);
				cubeFace.set_global_node_index(n, globalNodeIndex);
			}
		}

		// Fill in the labels for each node.
		GlobalNodeTableT& globalNodeTable = m_data->global_node_table();
		if(cubeFace.is_used(CubeFace::TOP_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::TOP_NODE));
			n.sourcedLabels.insert(make_sourced_label(topLeftLabel, topLeftLoc));
			n.sourcedLabels.insert(make_sourced_label(topRightLabel, topRightLoc));
		}
		if(cubeFace.is_used(CubeFace::LEFT_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::LEFT_NODE));
			n.sourcedLabels.insert(make_sourced_label(topLeftLabel, topLeftLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomLeftLabel, bottomLeftLoc));
		}
		if(cubeFace.is_used(CubeFace::MIDDLE_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::MIDDLE_NODE));
			n.sourcedLabels.insert(make_sourced_label(topLeftLabel, topLeftLoc));
			n.sourcedLabels.insert(make_sourced_label(topRightLabel, topRightLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomLeftLabel, bottomLeftLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomRightLabel, bottomRightLoc));
		}
		if(cubeFace.is_used(CubeFace::RIGHT_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::RIGHT_NODE));
			n.sourcedLabels.insert(make_sourced_label(topRightLabel, topRightLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomRightLabel, bottomRightLoc));
		}
		if(cubeFace.is_used(CubeFace::BOTTOM_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::BOTTOM_NODE));
			n.sourcedLabels.insert(make_sourced_label(bottomLeftLabel, bottomLeftLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomRightLabel, bottomRightLoc));
		}

		// Run through the edges and fill in the adjacent node entries in the global nodes.
		// Note that the edge endpoints are *local* indices, so they need to be mapped to
		// global indices before being stored in the global nodes.
		for(std::list<CubeFace::Edge>::const_iterator it=edges.begin(), iend=edges.end(); it!=iend; ++it)
		{
			int u = cubeFace.global_node_index(it->u);
			int v = cubeFace.global_node_index(it->v);
			globalNodeTable(u).adjacentNodes.insert(v);
			globalNodeTable(v).adjacentNodes.insert(u);
		}

		// Fill in the cube face in the cube face table.
		m_data->cube_face_table().set_cube_face(m_x, m_y, m_z, m_faceDesignator, cubeFace);
	}

	Label label(const Vector3i& loc) const
	{
		itk::Index<3> index = {{loc.x, loc.y, loc.z}};
		return m_labelling->GetPixel(index);
	}
};

}

#endif
