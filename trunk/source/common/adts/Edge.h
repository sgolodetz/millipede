/***
 * millipede: Edge.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_EDGE
#define H_MILLIPEDE_EDGE

namespace mp {

template <typename T = int>
struct Edge
{
	//#################### PUBLIC VARIABLES ####################
	T u;
	T v;

	//#################### CONSTRUCTORS ####################
	Edge(T u_, T v_)
	:	u(u_), v(v_)
	{}
};

}

#endif
