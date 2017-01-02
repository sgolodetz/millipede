/***
 * millipede: DeepWaterfallPass.h
 * Copyright Stuart Golodetz, 2017. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DEEPWATERFALLPASS
#define H_MILLIPEDE_DEEPWATERFALLPASS

#include <algorithm>
#include <list>

#include "WaterfallPass.h"

namespace mp {

template <typename EdgeWeight>
class DeepWaterfallPass : public WaterfallPass<EdgeWeight>
{
	//#################### PRIVATE VARIABLES ####################
private:
	bool m_haveMerged;

	//#################### PUBLIC METHODS ####################
public:
	RootedMST<EdgeWeight>& run(RootedMST<EdgeWeight>& mst)
	{
		// Find the weight of the lowest-weighted edge(s) in the MST.
		EdgeWeight lowestWeight = INT_MAX;
		for(typename RootedMST<EdgeWeight>::EdgeConstIterator it = mst.edges_cbegin(), iend = mst.edges_cend(); it != iend; ++it)
		{
			if(it->weight < lowestWeight)
			{
				lowestWeight = it->weight;
			}
		}

		// Merge an edge with that lowest weight.
		m_haveMerged = false;
		run_sub(mst, mst.tree_root(), lowestWeight);

		return mst;
	}

	//#################### PRIVATE METHODS ####################
private:
	int run_sub(RootedMST<EdgeWeight>& mst, int parent, EdgeWeight lowestWeight)
	{
		std::set<int> children = mst.tree_children(parent);
		if(children.size() > 0)
		{
			// Recurse on all the children.
			std::list<int> results;
			for(std::set<int>::const_iterator it = children.begin(), iend = children.end(); it != iend; ++it)
			{
				results.push_back(run_sub(mst, *it, lowestWeight));
			}

			// Merge any descending edges whose weight is the lowest weight.
			for(std::list<int>::const_iterator it = results.begin(), iend = results.end(); it != iend; ++it)
			{
				if(!m_haveMerged && mst.edge_weight(parent, *it) == lowestWeight)
				{
					parent = this->merge_nodes(mst, parent, *it);
					m_haveMerged = true;
				}
			}
		}

		return parent;
	}
};

}

#endif
