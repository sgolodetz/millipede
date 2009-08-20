/***
 * millipede: PFWaterfallEdge.tpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#define PFWaterfallEdge_HEADER		template <typename BranchProperties, typename LeafProperties, typename IDConverter>
#define PFWaterfallEdge_THIS		PFWaterfallEdge<BranchProperties, LeafProperties, IDConverter>

#include "Exception.h"

namespace mp {

//#################### CONSTRUCTORS ####################
PFWaterfallEdge_HEADER
PFWaterfallEdge_THIS::PFWaterfallEdge(int u, int v, int weight, const PF_Ptr& forest)
:	WaterfallEdge(weight), m_u(u), m_v(v), m_forest(forest)
{}

//#################### STATIC FACTORY METHODS ####################
PFWaterfallEdge_HEADER
template <typename Node, typename EdgeValue>
typename PFWaterfallEdge_THIS::PFWaterfallEdge_Ptr
PFWaterfallEdge_THIS::construct_waterfall_tree(const MinimumSpanningTree<Node,EdgeValue>& mst, const PF_Ptr& forest)
{
	typedef MinimumSpanningTree<Node,EdgeValue> MST;

	// Step 1:	Try and find a start node in the MST.
	MST::NodeCIterator_Ptr nodes = mst.nodes();
	if(!nodes->has_next()) throw Exception("Cannot construct a waterfall tree from an empty MST");

	int startNode = nodes->next().first;

	// Step 2:	Construct a dummy edge for the root of the waterfall tree.
	PFWaterfallEdge_Ptr root(new PFWaterfallEdge(-1, startNode, INT_MAX, forest));

	// Step 3:	Construct the waterfall subtree below the start node (this is actually the whole tree).
	construct_waterfall_subtree(startNode, root, mst, forest);

	return root;
}

PFWaterfallEdge_HEADER
template <typename Node, typename EdgeValue>
void PFWaterfallEdge_THIS::construct_waterfall_subtree(int curNode, PFWaterfallEdge_Ptr parentEdge, const MinimumSpanningTree<Node,EdgeValue>& mst, const PF_Ptr& forest)
{
	typedef MinimumSpanningTree<Node,EdgeValue> MST;

	// Step 1:	Get all the nodes adjacent to this one.
	MST::NodeCIterator_Ptr adjNodes = mst.adjacent_nodes(curNode);

	// Step 2:	Add a child edge for any node which isn't at the other end of our parent edge, and recurse.
	int fromNode = parentEdge->m_u;
	while(adjNodes->has_next())
	{
		int otherNode = adjNodes->next().first;
		if(otherNode != fromNode)
		{
			PFWaterfallEdge_Ptr child(new PFWaterfallEdge(curNode, otherNode, mst(curNode, otherNode), forest));
			parentEdge->add_child(child);
			construct_waterfall_subtree(otherNode, child, mst, forest);
		}
	}
}

//#################### PRIVATE METHODS ####################
PFWaterfallEdge_HEADER
void PFWaterfallEdge_THIS::merge_hook()
{
	m_forest->merge_tree_roots(m_u, m_v);
}

}

#undef PFWaterfallEdge_HEADER
#undef PFWaterfallEdge_THIS
