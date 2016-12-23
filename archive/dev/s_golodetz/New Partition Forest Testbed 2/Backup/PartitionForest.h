/***
 * millipede: PartitionForest.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFOREST
#define H_MILLIPEDE_PARTITIONFOREST

#include <set>
#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include "AdjacencyGraph.h"

namespace mp {

template <typename BranchProperties, typename LeafProperties, typename IDConverter>
class PartitionForest
{
	//#################### NESTED CLASSES ####################
public:
	struct NodeHandle
	{
		int layer;
		int index;

		NodeHandle(int layer_, int index_)
		:	layer(layer_), index(index_)
		{}
	};

	class Branch
	{
		//#################### PRIVATE VARIABLES ####################
	private:
		std::set<int> m_children;
		int m_parent;
		BranchProperties m_properties;

		//#################### CONSTRUCTORS ####################
	public:
		explicit Branch(const std::set<int>& children);
	};

	class Leaf
	{
		//#################### PRIVATE VARIABLES ####################
	private:
		int m_parent;
		LeafProperties m_properties;
	};

	class BranchLayer
	{
		//#################### PRIVATE VARIABLES ####################
	private:
		PartitionForest *m_base;
		int m_layer;

		//#################### CONSTRUCTORS ####################
	private:
		BranchLayer(PartitionForest *base, int layer);

		//#################### PUBLIC METHODS ####################
	public:
		void merge_nodes(int u, int v);
	};

	class LeafLayer
	{
		// TODO
	};

	//#################### TYPEDEFS ####################
public:
	typedef AdjacencyGraph<Branch,int> BranchLayerGraph;
	typedef shared_ptr<BranchLayerGraph> BranchLayerGraph_Ptr;
	typedef shared_ptr<BranchLayer> BranchLayer_Ptr;
	typedef shared_ptr<const BranchLayer> BranchLayer_CPtr;
	typedef shared_ptr<LeafLayer> LeafLayer_Ptr;
	typedef shared_ptr<const LeafLayer> LeafLayer_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<BranchLayerGraph_Ptr> m_branchLayerGraphs;
	LeafLayer_Ptr m_leafLayer;

	//#################### CONSTRUCTORS ####################
public:
	PartitionForest(const std::vector<LeafProperties>& leafProperties, const BranchLayerGraph_Ptr& lowestBranchLayerGraph);

	//#################### PUBLIC METHODS ####################
public:
	BranchLayer_Ptr branch_layer(int layer);
	BranchLayer_CPtr branch_layer(int layer) const;
	void clone_layer_above(int layer);
	void clone_layer_below(int layer);
	LeafLayer_Ptr leaf_layer();
	LeafLayer_CPtr leaf_layer() const;
	// TODO
};

}

#include "PartitionForest.tpp"

#endif
