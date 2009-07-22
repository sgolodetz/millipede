/***
 * millipede: Waterfall.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Waterfall.h"
using namespace mp;

namespace {

//#################### LOCAL CLASSES ####################
struct Result
{
	WaterfallEdge_CPtr edge;
	Waterfall::Flag flag;

	Result(WaterfallEdge_CPtr edge_, Waterfall::Flag flag_)
	:	edge(edge_), flag(flag_)
	{}
};

//#################### LOCAL OPERATORS ####################
bool operator<(const Result& lhs, const Result& rhs)
{
	return	lhs.edge->weight() < rhs.edge->weight() ||
			(lhs.edge->weight() == rhs.edge->weight() && lhs.flag < rhs.flag);
}

}

namespace mp {

//#################### PUBLIC METHODS ####################
Waterfall::Flag Waterfall::iterate(WaterfallEdge_CPtr parent)
{
	const std::list<WaterfallEdge_CPtr>& children = parent->children();
	if(children.size() > 0)
	{
		// Run the waterfall recursively on all the children.
		std::list<Result> results;
		for(std::list<WaterfallEdge_CPtr>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			results.push_back(Result(*it, iterate(*it)));
		}

		// Sort the results by ascending weight and then flag (GR before LEQ).
		results.sort();

		// Calculate the flag to return.
		Result& lowest = *results.begin();
		Flag parentFlag = (parent->weight() <= lowest.edge->weight()) ? FLAG_LEQ : FLAG_GR;

		// If the weight of the lowest result is strictly less than that of the parent, merge it regardless of its own flag.
		if(parentFlag == FLAG_GR)
		{
			lowest.edge->merge();
			results.pop_front();
		}

		// Merge all other edges which have a LEQ flag.
		for(std::list<Result>::const_iterator it=results.begin(), iend=results.end(); it!=iend; ++it)
		{
			if(it->flag == FLAG_LEQ)
			{
				it->edge->merge();
			}
		}

		return parentFlag;
	}
	else return FLAG_LEQ; 
}

}
