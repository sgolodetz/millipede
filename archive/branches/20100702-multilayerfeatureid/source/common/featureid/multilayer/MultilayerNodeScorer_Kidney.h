/***
 * millipede: MultilayerNodeScorer_Kidney.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MULTILAYERNODESCORER_KIDNEY
#define H_MILLIPEDE_MULTILAYERNODESCORER_KIDNEY

#include "MultilayerNodeScorer.h"

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class MultilayerNodeScorer_Kidney : public MultilayerNodeScorer<LeafLayer,BranchLayer>
{
	//#################### TYPEDEFS ####################
private:
	typedef typename MultilayerNodeScorer<LeafLayer,BranchLayer>::BranchProperties BranchProperties;
	typedef typename MultilayerNodeScorer<LeafLayer,BranchLayer>::PartitionForest_CPtr PartitionForest_CPtr;
	
	//#################### CONSTRUCTORS ####################
public:
	explicit MultilayerNodeScorer_Kidney(const PartitionForest_CPtr& forest)
	:	MultilayerNodeScorer<LeafLayer,BranchLayer>(forest)
	{}

	//#################### PRIVATE METHODS ####################
private:
	double calculate_score(const BranchProperties& properties) const
	{
#if 1
		return this->gaussian_minmax(properties.mean_grey_value(), 165, 175);
#else
		double offset = fabs(properties.mean_grey_value() - 170);
		return std::max(1 - offset / 20.0, 0.0);
#endif
	}
};

}

#endif
