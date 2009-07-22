/***
 * millipede: Waterfall.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_WATERFALL
#define H_MILLIPEDE_WATERFALL

#include "WaterfallEdge.h"

namespace mp {

class Waterfall
{
	//#################### ENUMERATIONS ####################
public:
	enum Flag
	{
		FLAG_GR		= 0,
		FLAG_LEQ	= 1
	};

	//#################### PUBLIC METHODS ####################
public:
	/**
	Runs a waterfall iteration on the minimum spanning tree rooted at the specified parent edge.

	@param parent	The root of the MST on which to run the waterfall iteration
	@return			FLAG_LEQ if the weight on the parent edge is <= the weights on all its children, or FLAG_GR otherwise
	*/
	static Flag iterate(WaterfallEdge_CPtr parent);
};

}

#endif
