/***
 * millipede: AbdominalFeature.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "AbdominalFeature.h"

namespace mp {

template <>
std::vector<AbdominalFeature::Enum> feature_types()
{
	std::vector<AbdominalFeature::Enum> ret(AbdominalFeature::COUNT);
	for(int i=0; i<AbdominalFeature::COUNT; ++i) ret[i] = AbdominalFeature::Enum(i);
	return ret;
}

}
