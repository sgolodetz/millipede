/***
 * millipede: MarcoteguiWaterfallPass.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MARCOTEGUIWATERFALLPASS
#define H_MILLIPEDE_MARCOTEGUIWATERFALLPASS

#include <millipede/adts/PriorityQueue.h>
#include <millipede/util/NullType.h>
#include "GolodetzWaterfallPass.h"

namespace mp {

template <typename EdgeWeight>
class MarcoteguiWaterfallPass : public WaterfallPass<EdgeWeight>
{
	//#################### TYPEDEFS ####################
private:
	typedef GolodetzWaterfallPass<EdgeWeight> GolodetzWaterfallPassT;
	typedef typename GolodetzWaterfallPassT::NodeData GolodetzNodeData;
	typedef PriorityQueue<int,EdgeWeight,NullType> PQ;

	//#################### ENUMERATIONS ####################
private:
	enum NodeFlag
	{
		UNMARKED,
		MARKED,
	};

	//#################### NESTED CLASSES ####################
private:
	struct NodeData
	{
		NodeFlag m_nodeFlag;
		bool m_parentWillMerge;

		NodeData(NodeFlag nodeFlag, bool parentWillMerge)
		:	m_nodeFlag(nodeFlag), m_parentWillMerge(parentWillMerge)
		{}
	};

	//#################### PUBLIC METHODS ####################
public:
	RootedMST<EdgeWeight>& run(RootedMST<EdgeWeight>& mst)
	{
		// Run a Golodetz waterfall pass on the MST (without doing any merging) in order to classify the edges.
		GolodetzWaterfallPassT().run_without_merge_pass(mst);

		// Mark (and record) any edges which are part of a local minimum. Note that we represent the edges by their child node in the tree.
		std::list<int> localMinima;
		mark_local_minima(mst, mst.tree_root(), localMinima);

		// Build the initial propagation queue from the unmarked edges adjacent to the local minima.
		PQ pq = build_initial_propagation_queue(mst, localMinima);

		// Propagate the markers.
		propagate_markers(pq, mst);

		// Actually merge all the marked edges.
		merge_pass(mst, mst.tree_root());

		return mst;
	}

	//#################### PRIVATE METHODS ####################
private:
	static std::vector<int> adjacent_edges(int e, const RootedMST<EdgeWeight>& mst)
	{
		// The adjacent edges (noting that they are referred to by their child nodes in the tree) are
		// given by the union of those nodes directly adjacent to us and the children (excluding us)
		// of our parent in the tree.
		std::vector<int> adjEdges = mst.adjacent_nodes(e);
		std::set<int> otherChildren = mst.tree_children(mst.tree_parent(e));
		otherChildren.erase(e);
		std::copy(otherChildren.begin(), otherChildren.end(), std::back_inserter(adjEdges));
		return adjEdges;
	}

	static PQ build_initial_propagation_queue(const RootedMST<EdgeWeight>& mst, const std::list<int>& localMinima)
	{
		PQ pq;
		for(std::list<int>::const_iterator it=localMinima.begin(), iend=localMinima.end(); it!=iend; ++it)
		{
			enqueue_relevant_adjacent_edges(*it, pq, mst);
		}
		return pq;
	}

	static void enqueue_relevant_adjacent_edges(int e, PQ& pq, const RootedMST<EdgeWeight>& mst)
	{
		std::vector<int> adjEdges = adjacent_edges(e, mst);
		for(std::vector<int>::const_iterator it=adjEdges.begin(), iend=adjEdges.end(); it!=iend; ++it)
		{
			int parent = mst.tree_parent(*it);
			if(parent != -1)
			{
				NodeFlag flag = mst.template node_data<NodeData>(*it).m_nodeFlag;
				NodeFlag parentFlag = mst.template node_data<NodeData>(parent).m_nodeFlag;
				if(flag == UNMARKED || parentFlag == UNMARKED)
				{
					EdgeWeight weight = mst.edge_weight(parent, *it);
					if(!pq.contains(*it)) pq.insert(*it, weight, NullType());
				}
			}
		}
	}

	static bool mark_local_minima(RootedMST<EdgeWeight>& mst, int cur, std::list<int>& localMinima)
	{
		// Recurse on the children.
		bool childEdgeIsLocalMinimum = false;	// are any of the child edges of this node a local minimum?
		std::set<int> children = mst.tree_children(cur);
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			bool result = mark_local_minima(mst, *it, localMinima);
			childEdgeIsLocalMinimum = childEdgeIsLocalMinimum || result;
		}

		// Check whether the parent edge of this node is a singular minimum, or part of a minimal plateau, and record it if so.
		bool parentEdgeIsLocalMinimum = false;
		const GolodetzNodeData& data = mst.template node_data<GolodetzNodeData>(cur);
		if(minimum_contribution(data.m_parentBottomClassifier) + minimum_contribution(data.m_parentTopClassifier) == 2)
		{
			parentEdgeIsLocalMinimum = true;
			localMinima.push_back(cur);
		}

		// Mark this node if its parent edge or one of its child edges is a local minimum. If the
		// parent edge is the local minimum, also set the parent edge to be merged later.
		if(parentEdgeIsLocalMinimum)		mst.set_node_data(cur, NodeData(MARKED, true));
		else if(childEdgeIsLocalMinimum)	mst.set_node_data(cur, NodeData(MARKED, false));
		else								mst.set_node_data(cur, NodeData(UNMARKED, false));

		return parentEdgeIsLocalMinimum;
	}

	int merge_pass(RootedMST<EdgeWeight>& mst, int cur)
	{
		// Recurse on the children.
		std::set<int> children = mst.tree_children(cur);
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			cur = merge_pass(mst, *it);
		}

		// Where necessary, merge the parent edge of this node (if any).
		NodeData curData = mst.template node_data<NodeData>(cur);
		int parent = mst.tree_parent(cur);
		if(curData.m_parentWillMerge)
		{
			NodeData parentData = mst.template node_data<NodeData>(parent);
			parent = this->merge_nodes(mst, parent, cur);

			// Note: Since the merging is unpredictable, we need to restore the parent's data afterwards.
			mst.set_node_data(parent, parentData);
		}

		return parent;
	}

	static int minimum_contribution(typename GolodetzWaterfallPassT::NodeClassifier nc)
	{
		return (nc == GolodetzWaterfallPassT::AMBIGUOUS_IN || nc == GolodetzWaterfallPassT::NO_FLOW || nc == GolodetzWaterfallPassT::UNAMBIGUOUS_IN) ? 1 : 0;
	}

	static void propagate_markers(PQ& pq, RootedMST<EdgeWeight>& mst)
	{
		while(!pq.empty())
		{
			typename PQ::Element e = pq.top();
			pq.pop();

			int cur = e.id(), parent = mst.tree_parent(e.id());
			NodeData curData = mst.template node_data<NodeData>(cur), parentData = mst.template node_data<NodeData>(parent);
			if(curData.m_nodeFlag != MARKED || parentData.m_nodeFlag != MARKED)
			{
				curData.m_nodeFlag = parentData.m_nodeFlag = MARKED;
				curData.m_parentWillMerge = true;
				mst.set_node_data(cur, curData);
				mst.set_node_data(parent, parentData);
				enqueue_relevant_adjacent_edges(cur, pq, mst);
			}
		}
	}
};

}

#endif
