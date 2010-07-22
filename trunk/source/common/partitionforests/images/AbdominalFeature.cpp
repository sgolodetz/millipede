/***
 * millipede: AbdominalFeature.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "AbdominalFeature.h"

namespace mp {

template <> AbdominalFeature::Enum enum_begin()
{
	return AbdominalFeature::AORTA;
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

template <> std::map<AbdominalFeature::Enum,RGBA32> feature_colour_map()
{
	using namespace AbdominalFeature;
	using namespace ITKImageUtil;
	std::map<AbdominalFeature::Enum,RGBA32> m;
	m.insert(std::make_pair(AORTA,				make_rgba32(255,0,0,50)));
	m.insert(std::make_pair(INFERIOR_VENA_CAVA,	make_rgba32(0,0,255,50)));
	m.insert(std::make_pair(KIDNEY,				make_rgba32(255,255,0,50)));
	m.insert(std::make_pair(LIVER,				make_rgba32(128,0,128,50)));
	m.insert(std::make_pair(OTHER_ARTERY,		make_rgba32(128,0,0,50)));
	m.insert(std::make_pair(OTHER_VEIN,			make_rgba32(0,0,128,50)));
	m.insert(std::make_pair(RIB,				make_rgba32(192,255,192,100)));
	m.insert(std::make_pair(SPINAL_CORD,		make_rgba32(128,255,255,100)));
	m.insert(std::make_pair(SPLEEN,				make_rgba32(0,255,0,50)));
	m.insert(std::make_pair(VERTEBRA,			make_rgba32(192,255,255,100)));
	return m;
}

std::string feature_key(AbdominalFeature::Enum e)
{
	using namespace AbdominalFeature;
	switch(e)
	{
		case AORTA:					return "A";
		case INFERIOR_VENA_CAVA:	return "V";
		case KIDNEY:				return "K";
		case LIVER:					return "L";
		case RIB:					return "R";
		case SPINAL_CORD:			return "C";
		case SPLEEN:				return "P";
		case VERTEBRA:				return "S";
		default:					return "";
	}
}

std::string feature_name(AbdominalFeature::Enum e)
{
	using namespace AbdominalFeature;
	switch(e)
	{
		case AORTA:					return "Aorta";
		case INFERIOR_VENA_CAVA:	return "Inferior Vena Cava";
		case KIDNEY:				return "Kidney";
		case LIVER:					return "Liver";
		case OTHER_ARTERY:			return "Other Artery";
		case OTHER_VEIN:			return "Other Vein";
		case RIB:					return "Rib";
		case SPINAL_CORD:			return "Spinal Cord";
		case SPLEEN:				return "Spleen";
		case VERTEBRA:				return "Vertebra";
		default:					return "";
	}
}

std::string feature_shortcut(AbdominalFeature::Enum e)
{
	using namespace AbdominalFeature;
	switch(e)
	{
		case OTHER_ARTERY:	return "Shift+A";
		case OTHER_VEIN:	return "Shift+V";
		default:			return feature_key(e);
	}
}

}
