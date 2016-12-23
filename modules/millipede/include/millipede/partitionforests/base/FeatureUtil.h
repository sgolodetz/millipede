/***
 * millipede: FeatureUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FEATUREUTIL
#define H_MILLIPEDE_FEATUREUTIL

#include <map>
#include <string>

#include "../../util/ITKImageUtil.h"

namespace mp {

template <typename Feature> std::map<Feature,RGBA32> feature_colour_map();
template <typename Feature> Feature name_to_feature(const std::string& name);

}

#endif
