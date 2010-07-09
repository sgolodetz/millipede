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
	Vector3d position;
	std::set<SourcedLabel<Label> > sourcedLabels;
	bool valid;

	//#################### CONSTRUCTORS ####################
	explicit MeshNode(const Vector3d& position_)
	:	position(position_), valid(true)
	{}

	bool has_label(Label label) const
	{
		for(std::set<SourcedLabel<Label> >::const_iterator it=sourcedLabels.begin(), iend=sourcedLabels.end(); it!=iend; ++it)
		{
			if(it->label == label)
			{
				return true;
			}
		}
		return false;
	}
};

}

#endif
