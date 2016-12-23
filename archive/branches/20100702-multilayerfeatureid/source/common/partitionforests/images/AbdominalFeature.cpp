/***
 * millipede: AbdominalFeature.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "AbdominalFeature.h"

namespace mp {

template <> AbdominalFeature::Enum enum_begin()
{
	return AbdominalFeature::KIDNEY;
}

template <> AbdominalFeature::Enum enum_end()
{
	return AbdominalFeature::COUNT;
}

AbdominalFeature::Enum& operator++(AbdominalFeature::Enum& e)
{
	e = AbdominalFeature::Enum(e + 1);
	return e;
}

std::string feature_key(AbdominalFeature::Enum e)
{
	using namespace AbdominalFeature;
	switch(e)
	{
		case KIDNEY:	return "K";
		case LIVER:		return "L";
		default:		return "";
	}
}

std::string feature_name(AbdominalFeature::Enum e)
{
	using namespace AbdominalFeature;
	switch(e)
	{
		case KIDNEY:	return "Kidney";
		case LIVER:		return "Liver";
		default:		return "";
	}
}

}
