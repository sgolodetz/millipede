/***
 * millipede: NullType.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_NULLTYPE
#define H_MILLIPEDE_NULLTYPE

namespace mp {

/**
@brief	NullType is an empty type that is occasionally useful as a template argument (for instance when a class template
		provides the option to specify some auxiliary data and there's no use for that in a given bit of code).
*/
struct NullType {};

}

#endif
