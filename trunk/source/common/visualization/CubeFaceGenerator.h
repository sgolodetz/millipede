/***
 * millipede: CubeFaceGenerator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBEFACEGENERATOR
#define H_MILLIPEDE_CUBEFACEGENERATOR

#include <set>

#include <common/adts/Edge.h>
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
	typedef Edge<CubeFace::FaceNodeDesignator> EdgeT;
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
	static std::list<EdgeT> edges_on_face(Label topleft, Label topright, Label bottomleft, Label bottomright)
	{
		std::list<EdgeT> edges;

		std::set<Label, PriorityPred> labels;
		labels.insert(topleft);
		labels.insert(topright);
		labels.insert(bottomleft);
		labels.insert(bottomright);

		const int labelCount = static_cast<int>(labels.size());

		switch(labelCount)
		{
			case 1:
			case 2:
			{
				int a = topright != topleft, b = bottomleft != topleft, c = bottomright != topleft;
				int combo = (a << 2) + (b << 1) + c;
				switch(combo)
				{
					case 0:		// 0000 or 1111
						break;
					case 1:		// 0001 or 1110
						edges.push_back(EdgeT(CubeFace::RIGHT_NODE, CubeFace::BOTTOM_NODE));
						break;
					case 2:		// 0010 or 1101
						edges.push_back(EdgeT(CubeFace::LEFT_NODE, CubeFace::BOTTOM_NODE));
						break;
					case 3:		// 0011 or 1100
						edges.push_back(EdgeT(CubeFace::LEFT_NODE, CubeFace::RIGHT_NODE));
						break;
					case 4:		// 0100 or 1011
						edges.push_back(EdgeT(CubeFace::RIGHT_NODE, CubeFace::TOP_NODE));
						break;
					case 5:		// 0101 or 1010
						edges.push_back(EdgeT(CubeFace::TOP_NODE, CubeFace::BOTTOM_NODE));
						break;
					case 6:		// 0110 or 1001
						// Ambiguous case: use label priorities to resolve.
						if(PriorityPred()(topleft, topright))	// if the topleft label has priority over the topright one
						{
							edges.push_back(EdgeT(CubeFace::TOP_NODE, CubeFace::RIGHT_NODE));
							edges.push_back(EdgeT(CubeFace::LEFT_NODE, CubeFace::BOTTOM_NODE));
						}
						else
						{
							edges.push_back(EdgeT(CubeFace::TOP_NODE, CubeFace::LEFT_NODE));
							edges.push_back(EdgeT(CubeFace::RIGHT_NODE, CubeFace::BOTTOM_NODE));
						}
						break;
					case 7:		// 0111 or 1000
						edges.push_back(EdgeT(CubeFace::LEFT_NODE, CubeFace::TOP_NODE));
						break;
				}
				break;
			}
			case 3:
			{
				// Cases: tl=tr, tl=bl, tl=br, tr=bl, tr=br, bl=br
				if(topleft == topright)
				{
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(topleft == bottomleft)
				{
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(topleft == bottomright)
				{
					edges.push_back(EdgeT(CubeFace::TOP_NODE, CubeFace::RIGHT_NODE));
					edges.push_back(EdgeT(CubeFace::LEFT_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(topright == bottomleft)
				{
					edges.push_back(EdgeT(CubeFace::TOP_NODE, CubeFace::LEFT_NODE));
					edges.push_back(EdgeT(CubeFace::RIGHT_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(topright == bottomright)
				{
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				}
				else	// bottomleft == bottomright
				{
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
					edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
				}
				break;
			}
			default:
			{
				edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
				edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
				edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
				edges.push_back(EdgeT(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				break;
			}
		}

		return edges;
	}

	void execute_impl()
	{
		m_labelling = m_data->labelling();

		// Determine the locations of the face corners.
		Vector3i topleftLoc, toprightLoc, bottomleftLoc, bottomrightLoc;
		switch(m_faceDesignator)
		{
			case CubeFaceDesignator::FACE_XY:
				topleftLoc = Vector3i(m_x,m_y+1,m_z);		toprightLoc = Vector3i(m_x+1,m_y+1,m_z);
				bottomleftLoc = Vector3i(m_x,m_y,m_z);		bottomrightLoc = Vector3i(m_x+1,m_y,m_z);
				break;
			case CubeFaceDesignator::FACE_XZ:
				topleftLoc = Vector3i(m_x,m_y,m_z+1);		toprightLoc = Vector3i(m_x+1,m_y,m_z+1);
				bottomleftLoc = Vector3i(m_x,m_y,m_z);		bottomrightLoc = Vector3i(m_x+1,m_y,m_z);
				break;
			case CubeFaceDesignator::FACE_YZ:
				topleftLoc = Vector3i(m_x,m_y,m_z+1);		toprightLoc = Vector3i(m_x,m_y+1,m_z+1);
				bottomleftLoc = Vector3i(m_x,m_y,m_z);		bottomrightLoc = Vector3i(m_x,m_y+1,m_z);
				break;
			default:
				throw Exception("Invalid face designator");		// this should never happen
		}

		// Look up the labels of the face corners.
		Label topleftLabel = label(topleftLoc);
		Label toprightLabel = label(toprightLoc);
		Label bottomleftLabel = label(bottomleftLoc);
		Label bottomrightLabel = label(bottomrightLoc);

		// Calculate the edges on the face from the labels of the corners.
		std::list<EdgeT> edges = edges_on_face(topleftLabel, toprightLabel, bottomleftLabel, bottomrightLabel);

		// If there aren't any edges, this cube face is irrelevant to the mesh.
		if(edges.size() == 0) return;

		// Construct the cube face, marking each used local node to indicate that we need to look up its global node.
		CubeFace cubeFace;
		for(std::list<EdgeT>::const_iterator it=edges.begin(), iend=edges.end(); it!=iend; ++it)
		{
			cubeFace.set_used(it->u);
			cubeFace.set_used(it->v);
		}

		// Build the mapping from local node indices to global coordinates.
		Vector3i locs[CubeFace::POTENTIAL_NODE_COUNT];
		typename GlobalNodeTableT::NodeDesignator nodeDesignators[CubeFace::POTENTIAL_NODE_COUNT];
		switch(m_faceDesignator)
		{
			case CubeFaceDesignator::FACE_XY:
			{
				locs[CubeFace::TOP_NODE] = Vector3i(m_x,m_y+1,m_z);		nodeDesignators[CubeFace::TOP_NODE] = GlobalNodeTableT::NODE_100;
				locs[CubeFace::LEFT_NODE] = Vector3i(m_x,m_y,m_z);		nodeDesignators[CubeFace::LEFT_NODE] = GlobalNodeTableT::NODE_010;
				locs[CubeFace::MIDDLE_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::MIDDLE_NODE] = GlobalNodeTableT::NODE_110;
				locs[CubeFace::RIGHT_NODE] = Vector3i(m_x+1,m_y,m_z);	nodeDesignators[CubeFace::RIGHT_NODE] = GlobalNodeTableT::NODE_010;
				locs[CubeFace::BOTTOM_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::BOTTOM_NODE] = GlobalNodeTableT::NODE_100;
				break;
			}
			case CubeFaceDesignator::FACE_XZ:
			{
				locs[CubeFace::TOP_NODE] = Vector3i(m_x,m_y,m_z+1);		nodeDesignators[CubeFace::TOP_NODE] = GlobalNodeTableT::NODE_100;
				locs[CubeFace::LEFT_NODE] = Vector3i(m_x,m_y,m_z);		nodeDesignators[CubeFace::LEFT_NODE] = GlobalNodeTableT::NODE_001;
				locs[CubeFace::MIDDLE_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::MIDDLE_NODE] = GlobalNodeTableT::NODE_101;
				locs[CubeFace::RIGHT_NODE] = Vector3i(m_x+1,m_y,m_z);	nodeDesignators[CubeFace::RIGHT_NODE] = GlobalNodeTableT::NODE_001;
				locs[CubeFace::BOTTOM_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::BOTTOM_NODE] = GlobalNodeTableT::NODE_100;
				break;
			}
			case CubeFaceDesignator::FACE_YZ:
			{
				locs[CubeFace::TOP_NODE] = Vector3i(m_x,m_y,m_z+1);		nodeDesignators[CubeFace::TOP_NODE] = GlobalNodeTableT::NODE_010;
				locs[CubeFace::LEFT_NODE] = Vector3i(m_x,m_y,m_z);		nodeDesignators[CubeFace::LEFT_NODE] = GlobalNodeTableT::NODE_001;
				locs[CubeFace::MIDDLE_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::MIDDLE_NODE] = GlobalNodeTableT::NODE_011;
				locs[CubeFace::RIGHT_NODE] = Vector3i(m_x,m_y+1,m_z);	nodeDesignators[CubeFace::RIGHT_NODE] = GlobalNodeTableT::NODE_001;
				locs[CubeFace::BOTTOM_NODE] = Vector3i(m_x,m_y,m_z);	nodeDesignators[CubeFace::BOTTOM_NODE] = GlobalNodeTableT::NODE_010;
				break;
			}
		}

		// Lookup the global node indices.
		for(int i=0; i<CubeFace::POTENTIAL_NODE_COUNT; ++i)
		{
			CubeFace::FaceNodeDesignator n = static_cast<CubeFace::FaceNodeDesignator>(i);
			if(cubeFace.is_used(n))
			{
				int globalNodeIndex = m_data->global_node_table().find_index(locs[i], nodeDesignators[i]);
				cubeFace.set_global_node_index(n, globalNodeIndex);
			}
		}

		// Fill in the labels for each node.
		GlobalNodeTableT& globalNodeTable = m_data->global_node_table();
		if(cubeFace.is_used(CubeFace::TOP_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::TOP_NODE));
			n.sourcedLabels.insert(make_sourced_label(topleftLabel, topleftLoc));
			n.sourcedLabels.insert(make_sourced_label(toprightLabel, toprightLoc));
		}
		if(cubeFace.is_used(CubeFace::LEFT_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::LEFT_NODE));
			n.sourcedLabels.insert(make_sourced_label(topleftLabel, topleftLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomleftLabel, bottomleftLoc));
		}
		if(cubeFace.is_used(CubeFace::MIDDLE_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::MIDDLE_NODE));
			n.sourcedLabels.insert(make_sourced_label(topleftLabel, topleftLoc));
			n.sourcedLabels.insert(make_sourced_label(toprightLabel, toprightLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomleftLabel, bottomleftLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomrightLabel, bottomrightLoc));
		}
		if(cubeFace.is_used(CubeFace::RIGHT_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::RIGHT_NODE));
			n.sourcedLabels.insert(make_sourced_label(toprightLabel, toprightLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomrightLabel, bottomrightLoc));
		}
		if(cubeFace.is_used(CubeFace::BOTTOM_NODE))
		{
			MeshNodeT& n = globalNodeTable(cubeFace.global_node_index(CubeFace::BOTTOM_NODE));
			n.sourcedLabels.insert(make_sourced_label(bottomleftLabel, bottomleftLoc));
			n.sourcedLabels.insert(make_sourced_label(bottomrightLabel, bottomrightLoc));
		}

		// Run through the edges and fill in the adjacent node entries in the global nodes.
		// Note that the edge endpoints are *local* indices, so they need to be mapped to
		// global indices before being stored in the global nodes.
		for(std::list<EdgeT>::const_iterator it=edges.begin(), iend=edges.end(); it!=iend; ++it)
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
		itk::Index<3> index = {{loc.get<0>(), loc.get<1>(), loc.get<2>()}};
		return m_labelling->GetPixel(index);
	}
};

}

#endif
