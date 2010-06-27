/***
 * millipede: AbdominalFeature.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ABDOMINALFEATURE
#define H_MILLIPEDE_ABDOMINALFEATURE

#include <vector>

namespace mp {

struct AbdominalFeature
{
	enum Enum
	{
		KIDNEY,
		LIVER,
		COUNT,
	};
};

template <typename Feature> std::vector<Feature> feature_types();
template <> std::vector<AbdominalFeature::Enum> feature_types();

}

#endif
