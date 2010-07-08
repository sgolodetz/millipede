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
	CubeFace m_cubeFace;
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
	/**
	@brief	Build the mapping that allows cube face nodes to be looked up in the global node table.
	*/
	void build_mapping_to_global_coordinates(std::vector<Vector3i>& nodeLocs, std::vector<typename GlobalNodeTableT::NodeDesignator>& nodeDesignators) const
	{
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
	}

	/**
	@brief	Constructs the cube face, marking which face nodes are being used based on the face edges.

	@param[in]	edges	The face edges
	@return	The constructed cube face
	*/
	CubeFace construct_initial_cube_face(const std::list<CubeFace::Edge>& edges) const
	{
		CubeFace cubeFace;
		for(std::list<CubeFace::Edge>::const_iterator it=edges.begin(), iend=edges.end(); it!=iend; ++it)
		{
			cubeFace.set_used(it->u);
			cubeFace.set_used(it->v);
		}
		return cubeFace;
	}

	/**
	@brief	Calculates the locations of the four face vertices in the volume.

	@return	A std::vector containing the face vertex locations
	*/
	std::vector<Vector3i> determine_face_vertex_locations() const
	{
		std::vector<Vector3i> locs(CubeFace::POTENTIAL_NODE_COUNT);
		switch(m_faceDesignator)
		{
			case CubeFaceDesignator::FACE_XY:
				locs[CubeFace::TOP_LEFT_VERTEX] = Vector3i(m_x,m_y+1,m_z);		locs[CubeFace::TOP_RIGHT_VERTEX] = Vector3i(m_x+1,m_y+1,m_z);
				locs[CubeFace::BOTTOM_LEFT_VERTEX] = Vector3i(m_x,m_y,m_z);		locs[CubeFace::BOTTOM_RIGHT_VERTEX] = Vector3i(m_x+1,m_y,m_z);
				break;
			case CubeFaceDesignator::FACE_XZ:
				locs[CubeFace::TOP_LEFT_VERTEX] = Vector3i(m_x,m_y,m_z+1);		locs[CubeFace::TOP_RIGHT_VERTEX] = Vector3i(m_x+1,m_y,m_z+1);
				locs[CubeFace::BOTTOM_LEFT_VERTEX] = Vector3i(m_x,m_y,m_z);		locs[CubeFace::BOTTOM_RIGHT_VERTEX] = Vector3i(m_x+1,m_y,m_z);
				break;
			case CubeFaceDesignator::FACE_YZ:
				locs[CubeFace::TOP_LEFT_VERTEX] = Vector3i(m_x,m_y,m_z+1);		locs[CubeFace::TOP_RIGHT_VERTEX] = Vector3i(m_x,m_y+1,m_z+1);
				locs[CubeFace::BOTTOM_LEFT_VERTEX] = Vector3i(m_x,m_y,m_z);		locs[CubeFace::BOTTOM_RIGHT_VERTEX] = Vector3i(m_x,m_y+1,m_z);
				break;
			default:
				throw Exception("Invalid face designator");		// this should never happen
		}
		return locs;
	}

	/**
	@brief	Determines the pattern of edges on the face, based on the labels of the face vertices.

	@param[in]	labels	The labels on the face vertices
	@return	The edges induced by these labels
	*/
	static std::list<CubeFace::Edge> edges_on_face(const std::vector<Label>& labels)
	{
		std::list<CubeFace::Edge> edges;

		std::set<Label, PriorityPred> uniqueLabels(labels.begin(), labels.end());
		const size_t uniqueLabelCount = uniqueLabels.size();

		switch(uniqueLabelCount)
		{
			case 1:
			case 2:
			{
				int a = labels[CubeFace::TOP_RIGHT_VERTEX] != labels[CubeFace::TOP_LEFT_VERTEX];
				int b = labels[CubeFace::BOTTOM_LEFT_VERTEX] != labels[CubeFace::TOP_LEFT_VERTEX];
				int c = labels[CubeFace::BOTTOM_RIGHT_VERTEX] != labels[CubeFace::TOP_LEFT_VERTEX];
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
						if(PriorityPred()(labels[CubeFace::TOP_LEFT_VERTEX], labels[CubeFace::TOP_RIGHT_VERTEX]))
						{
							// The top left label has priority over the top right one.
							edges.push_back(CubeFace::Edge(CubeFace::TOP_NODE, CubeFace::RIGHT_NODE));
							edges.push_back(CubeFace::Edge(CubeFace::LEFT_NODE, CubeFace::BOTTOM_NODE));
						}
						else
						{
							// The top right label has priority over the top left one.
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
				if(labels[CubeFace::TOP_LEFT_VERTEX] == labels[CubeFace::TOP_RIGHT_VERTEX])
				{
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(labels[CubeFace::TOP_LEFT_VERTEX] == labels[CubeFace::BOTTOM_LEFT_VERTEX])
				{
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::RIGHT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(labels[CubeFace::TOP_LEFT_VERTEX] == labels[CubeFace::BOTTOM_RIGHT_VERTEX])
				{
					edges.push_back(CubeFace::Edge(CubeFace::TOP_NODE, CubeFace::RIGHT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::LEFT_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(labels[CubeFace::TOP_RIGHT_VERTEX] == labels[CubeFace::BOTTOM_LEFT_VERTEX])
				{
					edges.push_back(CubeFace::Edge(CubeFace::TOP_NODE, CubeFace::LEFT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::RIGHT_NODE, CubeFace::BOTTOM_NODE));
				}
				else if(labels[CubeFace::TOP_RIGHT_VERTEX] == labels[CubeFace::BOTTOM_RIGHT_VERTEX])
				{
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::TOP_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::LEFT_NODE));
					edges.push_back(CubeFace::Edge(CubeFace::MIDDLE_NODE, CubeFace::BOTTOM_NODE));
				}
				else	// bl=br
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

		std::vector<Vector3i> vertexLocs = determine_face_vertex_locations();

		std::vector<Label> vertexLabels(4);
		for(int i=0; i<4; ++i) vertexLabels[i] = label(vertexLocs[i]);

		std::list<CubeFace::Edge> edges = edges_on_face(vertexLabels);
		if(edges.size() == 0) return;	// if there aren't any edges, this cube face is irrelevant to the mesh

		m_cubeFace = construct_initial_cube_face(edges);
		fill_in_global_indices();
		fill_in_sourced_labels(vertexLabels, vertexLocs);
		fill_in_adjacent_nodes(edges);
		m_data->cube_face_table().set_cube_face(m_x, m_y, m_z, m_faceDesignator, m_cubeFace);
	}

	/**
	@brief	Updates the nodes adjacent to each global node, based on the edges found on this cube face.

	Note that the endpoints of the edges passed in are *local* to the cube face, so they will be
	mapped to global indices before being stored in the global nodes.

	@param[in]	edges	The edges on this cube face
	*/
	void fill_in_adjacent_nodes(const std::list<CubeFace::Edge>& edges)
	{
		GlobalNodeTableT& globalNodeTable = m_data->global_node_table();
		for(std::list<CubeFace::Edge>::const_iterator it=edges.begin(), iend=edges.end(); it!=iend; ++it)
		{
			int u = m_cubeFace.global_node_index(it->u);
			int v = m_cubeFace.global_node_index(it->v);
			globalNodeTable(u).adjacentNodes.insert(v);
			globalNodeTable(v).adjacentNodes.insert(u);
		}
	}

	/**
	@brief	Fills in the global indices of any used cube face nodes.
	*/
	void fill_in_global_indices()
	{
		std::vector<Vector3i> nodeLocs(CubeFace::POTENTIAL_NODE_COUNT);
		std::vector<typename GlobalNodeTableT::NodeDesignator> nodeDesignators(CubeFace::POTENTIAL_NODE_COUNT);
		build_mapping_to_global_coordinates(nodeLocs, nodeDesignators);

		for(CubeFace::NodeDesignator n=enum_begin<CubeFace::NodeDesignator>(), end=enum_end<CubeFace::NodeDesignator>(); n!=end; ++n)
		{
			if(m_cubeFace.is_used(n))
			{
				int globalNodeIndex = m_data->global_node_table().find_index(nodeLocs[n], nodeDesignators[n]);
				m_cubeFace.set_global_node_index(n, globalNodeIndex);
			}
		}
	}

	/**
	@brief	Fills in the sourced labels for each global node referenced by the cube face.

	Specifically, this involves selecting the face vertices that produce labels for a given node,
	and writing their labels and locations into the node structure.

	@param[in]	vertexLabels	The labels of the face vertices
	@param[in]	vertexLocs		The locations of the face vertices
	*/
	void fill_in_sourced_labels(const std::vector<Label>& vertexLabels, const std::vector<Vector3i>& vertexLocs)
	{
		GlobalNodeTableT& globalNodeTable = m_data->global_node_table();
		for(CubeFace::NodeDesignator n=enum_begin<CubeFace::NodeDesignator>(), end=enum_end<CubeFace::NodeDesignator>(); n!=end; ++n)
		{
			if(m_cubeFace.is_used(n))
			{
				MeshNodeT& node = globalNodeTable(m_cubeFace.global_node_index(n));
				const std::vector<CubeFace::VertexDesignator>& labelSources = label_sources(n);
				for(size_t i=0, size=labelSources.size(); i<size; ++i)
				{
					node.sourcedLabels.insert(make_sourced_label(vertexLabels[i], vertexLocs[i]));
				}
			}
		}
	}

	/**
	@brief	Returns the label of the specified location in the volume.

	@param[in]	loc		A location in the volume
	@return	As described
	*/
	Label label(const Vector3i& loc) const
	{
		itk::Index<3> index = {{loc.x, loc.y, loc.z}};
		return m_labelling->GetPixel(index);
	}

	/**
	@brief	Returns the set of face vertices that provide the labels for the specified face node.

	@param[in]	n	A face node designator
	@return	The set of face vertices that provide labels for n
	*/
	static const std::vector<CubeFace::VertexDesignator>& label_sources(CubeFace::NodeDesignator n)
	{
		static std::vector<std::vector<CubeFace::VertexDesignator> > labelSources(CubeFace::POTENTIAL_NODE_COUNT);
		static bool done = false;
		if(!done)
		{
			labelSources[CubeFace::TOP_NODE].push_back(CubeFace::TOP_LEFT_VERTEX);
			labelSources[CubeFace::TOP_NODE].push_back(CubeFace::TOP_RIGHT_VERTEX);
			labelSources[CubeFace::LEFT_NODE].push_back(CubeFace::TOP_LEFT_VERTEX);
			labelSources[CubeFace::LEFT_NODE].push_back(CubeFace::BOTTOM_LEFT_VERTEX);
			labelSources[CubeFace::MIDDLE_NODE].push_back(CubeFace::TOP_LEFT_VERTEX);
			labelSources[CubeFace::MIDDLE_NODE].push_back(CubeFace::TOP_RIGHT_VERTEX);
			labelSources[CubeFace::MIDDLE_NODE].push_back(CubeFace::BOTTOM_LEFT_VERTEX);
			labelSources[CubeFace::MIDDLE_NODE].push_back(CubeFace::BOTTOM_RIGHT_VERTEX);
			labelSources[CubeFace::RIGHT_NODE].push_back(CubeFace::TOP_RIGHT_VERTEX);
			labelSources[CubeFace::RIGHT_NODE].push_back(CubeFace::BOTTOM_RIGHT_VERTEX);
			labelSources[CubeFace::BOTTOM_NODE].push_back(CubeFace::BOTTOM_LEFT_VERTEX);
			labelSources[CubeFace::BOTTOM_NODE].push_back(CubeFace::BOTTOM_RIGHT_VERTEX);
			done = true;
		}
		return labelSources[n];
	}
};

}

#endif
