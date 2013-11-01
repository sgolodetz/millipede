/***
 * millipede: MergeUtil.h
 * Copyright Stuart Golodetz, 2013. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MERGEUTIL
#define H_MILLIPEDE_MERGEUTIL

#include <algorithm>
#include <map>
#include <utility>
#include <vector>

#include <common/partitionforests/base/PartitionForest.h>

namespace mp {

namespace mp_MergeUtil {

using boost::shared_ptr;

//#################### ENUMERATIONS ####################

/**
@brief	The speed at which two forests should be merged.

Partition forests can be merged at one of two different speeds. Fast merging involves
merging regions in the resulting forest as soon as they merge in either of the input
forests; slow merging involves merging them only once they have merged in both of the
input forests.
*/
enum MergeSpeed
{
	MERGE_FAST,
	MERGE_SLOW
};

//#################### TYPEDEFS ####################

/** A table mapping pairs of forest leaf indices to the index of the layer in which their ancestors merge. */
typedef std::map<std::pair<int,int>,int> MergeLayerTable;

/** A table mapping forest layer indices to the pairs of leaf nodes whose ancestors should be merged in that layer. */
typedef std::map<int,std::vector<std::pair<int,int> > > MergeTable;

//#################### FUNCTIONS ####################

/**
@brief	Finds the layer in which the two specified leaf nodes merge in the specified forest
		and fills in the corresponding entry in the merge layer table.

@param[in]		forest	The forest.
@param[in]		i		The index of the first leaf node.
@param[in]		j		The index of the second leaf node.
@param[in,out]	table	The merge layer table.
*/
template <typename LeafLayerType, typename BranchLayerType>
void find_merge_layer(const shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >& forest, int i, int j, MergeLayerTable& table)
{
	int curLayer = 0;
	std::pair<int,int> initial = std::make_pair(i, j);
	std::pair<int,int> cur = initial;
	while(cur.first != -1 && cur.second != -1)
	{
		if(cur.first > cur.second)
		{
			// The table is symmetric, so we store only ordered pairs to save space.
			std::swap(cur.first, cur.second);
		}
		else if(cur.first == cur.second)
		{
			// We have reached the merge layer of i and j, so store it in the table.
			table[initial] = curLayer;
			return;
		}

		// The merge layer of i and j is always equal to the merge layer of cur.first and
		// cur.second, so we can use existing results in the table to save some work.
		MergeLayerTable::const_iterator it = table.find(cur);
		if(it != table.end())
		{
			table[initial] = it->second;
			return;
		}

		// If we have not yet found the merge layer, keep walking up the forest.
		cur.first = forest->parent_of(PFNodeID(curLayer, cur.first)).index();
		cur.second = forest->parent_of(PFNodeID(curLayer, cur.second)).index();
		++curLayer;
	}

	// If i and j never merge in the forest, record this as a merge at "infinity".
	table[initial] = INT_MAX;
}

/**
@brief	Builds the merge layer table for the specified forest.

@param[in]	forest	The forest.
@return				The merge layer table.
*/
template <typename LeafLayerType, typename BranchLayerType>
MergeLayerTable build_merge_layer_table(const shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >& forest)
{
	MergeLayerTable result;
	int leafNodeCount = forest->leaf_node_count();
	for(int i=0; i<leafNodeCount-1; ++i)
	{
		for(int j=i+1; j<leafNodeCount; ++j)
		{
			find_merge_layer(forest, i, j, result);
		}
	}
	return result;
}

/**
@brief	Combines the merge layer tables from two forests to create a merge layer table
		that can be used to merge the two forests at the specified speed.

@param[in]	lhs		The merge layer table for one of the forests.
@param[in]	rhs		The merge layer table for the other forest.
@param[in]	speed	The speed at which we will want to merge the two forests.
@return				The combined merge layer table.
*/
MergeLayerTable combine_merge_layer_tables(const MergeLayerTable& lhs, const MergeLayerTable& rhs, MergeSpeed speed)
{
	MergeLayerTable result;

	// Loop through the two input tables in lock-step, keeping either the higher
	// or lower entry in each case based on the specified merge speed.
	MergeLayerTable::const_iterator it = lhs.begin(), iend = lhs.end(), jt = rhs.begin(), jend = rhs.end();
	while(it != iend && jt != jend)
	{
		bool keepLeft = speed == MERGE_SLOW ? it->second > jt->second : it->second < jt->second;
		result.insert(keepLeft ? *it : *jt);
		++it, ++jt;
	}

	return result;
}

/**
@brief	Builds a merge table from a merge layer table.

A merge table is the inverse of a merge layer table: it specifies the
pairs of nodes that should merge in each layer, whereas a merge layer
table specifies the layers in which pairs of nodes merge.

@param[in]	mlt		The merge layer table.
@return				The corresponding merge table.
*/
MergeTable build_merge_table(const MergeLayerTable& mlt)
{
	MergeTable result;
	for(MergeLayerTable::const_iterator it=mlt.begin(), iend=mlt.end(); it!=iend; ++it)
	{
		result[it->second].push_back(it->first);
	}
	return result;
}

/**
@brief	Merges the two input forests to create a new forest.

@param[in]	lhs		The left-hand input forest.
@param[in]	rhs		The right-hand input forest.
@param[in]	mlt		The merge layer table specifying how the two forests should be merged.
@return				The merged forest.
*/
template <typename LeafLayerType, typename BranchLayerType>
shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >
merge_forests(const shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >& lhs, const shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >& rhs, const MergeLayerTable& mlt)
{
	shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> > result(new PartitionForest<LeafLayerType,BranchLayerType>(lhs->copy_leaf_layer()));

	MergeTable mt = build_merge_table(mlt);
	for(int i=1; i<=lhs->highest_layer() || i<=rhs->highest_layer(); ++i)
	{
		result->clone_layer(i-1);

		const std::vector<std::pair<int,int> > merges = mt[i];
		for(size_t j=0, size=merges.size(); j<size; ++j)
		{
			PFNodeID lnode(0, merges[j].first), rnode(0, merges[j].second);
			lnode = result->ancestor_of(lnode, i);
			rnode = result->ancestor_of(rnode, i);

			std::set<int> nodes;
			nodes.insert(lnode.index());
			nodes.insert(rnode.index());
			if(result->are_connected(nodes, i))
			{
				std::set<PFNodeID> mergees;
				mergees.insert(lnode);
				mergees.insert(rnode);
				result->merge_sibling_nodes(mergees);
			}
			else
			{
				// If we can't yet merge the two specified nodes in this layer for
				// connectedness reasons, we should try again at the next layer up.
				mt[i+1].push_back(merges[j]);
			}
		}
	}

	return result;
}

/**
@brief	Merges the two input forests using the "fast merging" technique to create a new forest.

@param[in]	lhs		The left-hand input forest.
@param[in]	rhs		The right-hand input forest.
@return				The fast-merged forest.
*/
template <typename LeafLayerType, typename BranchLayerType>
shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >
fast_merge_forests(const shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >& lhs, const shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >& rhs)
{
	MergeLayerTable ltable = build_merge_layer_table(lhs);
	MergeLayerTable rtable = build_merge_layer_table(rhs);
	return merge_forests(lhs, rhs, combine_merge_layer_tables(ltable, rtable, MERGE_FAST));
}

/**
@brief	Merges the two input forests using the "slow merging" technique to create a new forest.

@param[in]	lhs		The left-hand input forest.
@param[in]	rhs		The right-hand input forest.
@return				The slow-merged forest.
*/
template <typename LeafLayerType, typename BranchLayerType>
shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >
slow_merge_forests(const shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >& lhs, const shared_ptr<PartitionForest<LeafLayerType,BranchLayerType> >& rhs)
{
	MergeLayerTable ltable = build_merge_layer_table(lhs);
	MergeLayerTable rtable = build_merge_layer_table(rhs);
	return merge_forests(lhs, rhs, combine_merge_layer_tables(ltable, rtable, MERGE_SLOW));
}

}

using mp_MergeUtil::fast_merge_forests;
using mp_MergeUtil::slow_merge_forests;

}

#endif

