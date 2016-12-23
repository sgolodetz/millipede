/***
 * millipede: PartitionForest.tpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#define PartitionForest_HEADER	template <typename BranchProperties, typename LeafProperties, typename IDConverter>
#define PartitionForest_THIS	PartitionForest<BranchProperties,LeafProperties,IDConverter>

namespace mp {

//#################### Branch - CONSTRUCTORS ####################
PartitionForest_HEADER
PartitionForest_THIS::Branch::Branch(const std::set<int>& children)
:	m_children(children), m_parent(-1)
{}

}
