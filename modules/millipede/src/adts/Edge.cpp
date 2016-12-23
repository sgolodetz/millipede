/***
 * millipede: Edge.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "adts/Edge.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Edge::Edge(int u_, int v_)
:	u(u_), v(v_)
{}

//#################### PREDICATES ####################
bool UndirectedEdgePredicate::operator()(const Edge& lhs, const Edge& rhs) const
{
	// This is written this way to ensure that Edge(u,v) and Edge(v,u) are equivalent in associative containers.
	const int& lSmaller = lhs.u < lhs.v ? lhs.u : lhs.v;
	const int& lLarger = lhs.u > lhs.v ? lhs.u : lhs.v;
	const int& rSmaller = rhs.u < rhs.v ? rhs.u : rhs.v;
	const int& rLarger = rhs.u > rhs.v ? rhs.u : rhs.v;
	return	(lSmaller < rSmaller) ||
			(lSmaller == rSmaller && lLarger < rLarger);
}

}
