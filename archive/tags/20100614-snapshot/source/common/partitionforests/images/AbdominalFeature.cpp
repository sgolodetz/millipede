/***
 * millipede: AbdominalFeature.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "AbdominalFeature.h"

namespace mp {

template <>
std::vector<AbdominalFeature> feature_types()
{
	std::vector<AbdominalFeature> ret(AF_COUNT);
	for(int i=0; i<AF_COUNT; ++i) ret[i] = AbdominalFeature(i);
	return ret;
}

}
