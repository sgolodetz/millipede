/***
 * millipede: PFWaterfallEdge.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PFWATERFALLEDGE
#define H_MILLIPEDE_PFWATERFALLEDGE

#include <list>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include "MinimumSpanningTree.h"
#include "PartitionForest.h"
#include "WaterfallEdge.h"

namespace mp {

template <typename BranchProperties, typename LeafProperties, typename IDConverter>
class PFWaterfallEdge : public WaterfallEdge
{
	//#################### TYPEDEFS ####################
private:
	typedef shared_ptr<PFWaterfallEdge> PFWaterfallEdge_Ptr;

	typedef PartitionForest<BranchProperties, LeafProperties, IDConverter> PF;
	typedef shared_ptr<PF> PF_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_u, m_v;
	PF_Ptr m_forest;

	//#################### CONSTRUCTORS ####################
public:
	PFWaterfallEdge(int u, int v, int weight, const PF_Ptr& forest);

	//#################### STATIC FACTORY METHODS ####################
public:
	template <typename Node, typename EdgeValue>
	static PFWaterfallEdge_Ptr construct_waterfall_tree(const MinimumSpanningTree<Node,EdgeValue>& mst, const PF_Ptr& forest);

private:
	template <typename Node, typename EdgeValue>
	static void construct_waterfall_subtree(int curNode, PFWaterfallEdge_Ptr parentEdge, const MinimumSpanningTree<Node,EdgeValue>& mst, const PF_Ptr& forest);

	//#################### PRIVATE METHODS ####################
private:
	void merge_hook();
};

}

#include "PFWaterfallEdge.tpp"

#endif
