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
	int u;
	int v;

	//#################### CONSTRUCTORS ####################
	Edge(int u_, int v_)
	:	u(u_), v(v_)
	{}
};

}

#endif
