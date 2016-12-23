/***
 * millipede: Edge.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_EDGE
#define H_MILLIPEDE_EDGE

namespace mp {

struct Edge
{
	//#################### PUBLIC VARIABLES ####################
	int u, v;

	//#################### CONSTRUCTORS ####################
	Edge(int u_, int v_);
};

//#################### PREDICATES ####################
struct UndirectedEdgePredicate
{
	bool operator()(const Edge& lhs, const Edge& rhs) const;
};

}

#endif
