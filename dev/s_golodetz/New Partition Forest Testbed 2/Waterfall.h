/***
 * millipede: Waterfall.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_WATERFALL
#define H_MILLIPEDE_WATERFALL

#include "WaterfallEdge.h"

namespace mp {

struct Waterfall
{
	//#################### ENUMERATIONS ####################
	enum Flag
	{
		FLAG_GEQ	= 0,
		FLAG_L		= 1
	};

	//#################### PUBLIC METHODS ####################
	/**
	Runs a waterfall iteration on the minimum spanning tree rooted at the specified parent edge.

	@param parent	The root of the MST on which to run the waterfall iteration
	@return			FLAG_L if the weight on the parent edge is < the weights on all its children, or FLAG_GEQ otherwise
	*/
	static Flag iterate(WaterfallEdge_Ptr parent);
};

}

#endif
