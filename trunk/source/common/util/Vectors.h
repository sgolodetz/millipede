/***
 * millipede: Vectors.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VECTORS
#define H_MILLIPEDE_VECTORS

#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>

namespace mp {

typedef boost::tuples::tuple<double,double,double> Vector3d;
typedef boost::tuples::tuple<int,int,int> Vector3i;

}

#endif
