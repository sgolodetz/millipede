/***
 * millipede: AbdominalFeature.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ABDOMINALFEATURE
#define H_MILLIPEDE_ABDOMINALFEATURE

#include <vector>

namespace mp {

enum AbdominalFeature
{
	AF_KIDNEY,
	AF_LIVER,
	AF_COUNT,
};

template <typename Feature> std::vector<Feature> feature_types();
template <> std::vector<AbdominalFeature> feature_types();

}

#endif
