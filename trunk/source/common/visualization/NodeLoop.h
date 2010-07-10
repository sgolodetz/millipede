/***
 * millipede: NodeLoop.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_NODELOOP
#define H_MILLIPEDE_NODELOOP

#include <utility>

namespace mp {

template <typename Label>
struct NodeLoop
{
	//#################### PUBLIC VARIABLES ####################
	std::vector<int> indices;
	std::pair<Label,Label> labels;

	//#################### CONSTRUCTORS ####################
	NodeLoop(const std::vector<int>& indices_, const std::pair<Label,Label>& labels_)
	:	indices(indices_), labels(labels_)
	{}
};

}

#endif
