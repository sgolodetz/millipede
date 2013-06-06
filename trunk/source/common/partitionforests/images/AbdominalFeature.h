/***
 * millipede: AbdominalFeature.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ABDOMINALFEATURE
#define H_MILLIPEDE_ABDOMINALFEATURE

#include <vector>

#include <common/partitionforests/base/FeatureUtil.h>
#include <common/util/EnumUtil.h>

namespace mp {

namespace AbdominalFeature {

enum Enum
{
	AORTA,
	INFERIOR_VENA_CAVA,
	KIDNEY,
	KIDNEY_LEFT,
	KIDNEY_RIGHT,
	LIVER,
	OTHER_ARTERY,
	OTHER_VEIN,
	RIB,
	SPINAL_CORD,
	SPLEEN,
	VERTEBRA,
	//MARKER,
	COUNT,
};

}

template <> AbdominalFeature::Enum enum_begin();
template <> AbdominalFeature::Enum enum_end();
AbdominalFeature::Enum& operator++(AbdominalFeature::Enum& e);

template <> std::map<AbdominalFeature::Enum,RGBA32> feature_colour_map();
std::string feature_key(AbdominalFeature::Enum e);
std::string feature_shortcut(AbdominalFeature::Enum e);
std::string feature_to_name(AbdominalFeature::Enum e);
template <> AbdominalFeature::Enum name_to_feature(const std::string& name);

}

#endif
