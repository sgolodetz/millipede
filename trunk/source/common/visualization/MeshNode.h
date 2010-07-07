/***
 * millipede: MeshNode.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHNODE
#define H_MILLIPEDE_MESHNODE

#include <set>

#include "SourcedLabel.h"

namespace mp {

template <typename Label>
struct MeshNode
{
	//#################### PUBLIC VARIABLES ####################
	std::set<int> adjacentNodes;
	itk::Vector<double,3> position;
	std::set<SourcedLabel<Label> > sourcedLabels;
	bool valid;

	//#################### CONSTRUCTORS ####################
	explicit MeshNode(const itk::Vector<double,3>& position_)
	:	position(position_), valid(true)
	{}
};

}

#endif
