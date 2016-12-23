/***
 * millipede: WeightedEdge.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_WEIGHTEDEDGE
#define H_MILLIPEDE_WEIGHTEDEDGE

#include <ostream>

namespace mp {

template <typename Weight>
struct WeightedEdge
{
	//#################### PUBLIC VARIABLES ####################
	int u;
	int v;
	Weight weight;

	//#################### CONSTRUCTORS ####################
	WeightedEdge(int u_, int v_, Weight weight_)
	:	u(u_), v(v_), weight(weight_)
	{}
};

//#################### GLOBAL OPERATORS ####################
template <typename Weight>
std::ostream& operator<<(std::ostream& os, const WeightedEdge<Weight>& e)
{
	os << "({" << e.u << ' ' << e.v << "}, " << e.weight << ')';
	return os;
}

}

#endif
