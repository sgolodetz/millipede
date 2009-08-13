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
private:
	class Node;

public:
	class Branch;
	class Leaf;
	struct NodeHandle;

	//#################### TYPEDEFS ####################
private:
	typedef shared_ptr<Node> Node_Ptr;
	typedef shared_ptr<const Node> Node_CPtr;

public:
	typedef AdjacencyGraph<Branch,int> BranchLayer;
	typedef shared_ptr<BranchLayer> BranchLayer_Ptr;
	typedef shared_ptr<const BranchLayer> BranchLayer_CPtr;
	typedef IAdjacencyGraph<Leaf,int> LeafLayer;
	typedef shared_ptr<LeafLayer> LeafLayer_Ptr;
	typedef shared_ptr<const LeafLayer> LeafLayer_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	LeafLayer_Ptr m_leafLayer;
	std::vector<BranchLayer_Ptr> m_branchLayers;

	//#################### CONSTRUCTORS ####################
public:
	PartitionForest(const LeafLayer_Ptr& leafLayer, const BranchLayer_Ptr& lowestBranchLayer);

	//#################### PUBLIC METHODS ####################
public:
	void clone_above_layer(int layer);
	void clone_below_layer(int layer);
	const Branch& get_branch(const NodeHandle& nh) const;
	const BranchLayer& get_branch_layer(int layer) const;
	const Leaf& get_leaf(int index) const;
	const LeafLayer& get_leaf_layer() const;
	const Node& get_node(const NodeHandle& nh) const;
	int highest_layer() const;
	void merge_siblings(int layer, const std::vector<int>& indices);
	void merge_tree_roots(int u, int v);

	//#################### PRIVATE METHODS ####################
private:
	void check_branch_layer(int layer) const;
	bool has_branch_layer(int layer) const;
	const Branch& lookup_branch(const NodeHandle& nh) const;
	void recalculate_properties(const NodeHandle& nh);
};

}

#include "PartitionForest.tpp"

#endif
