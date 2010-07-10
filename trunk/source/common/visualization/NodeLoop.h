/***
 * millipede: NodeLoop.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_NODELOOP
#define H_MILLIPEDE_NODELOOP

#include <set>
#include <vector>

namespace mp {

template <typename Label>
struct NodeLoop
{
	//#################### PUBLIC VARIABLES ####################
	std::vector<int> indices;
	std::set<Label> labels;

	//#################### CONSTRUCTORS ####################
	NodeLoop(const std::vector<int>& indices_, const std::set<Label>& labels_)
	:	indices(indices_), labels(labels_)
	{}
};

}

#endif
