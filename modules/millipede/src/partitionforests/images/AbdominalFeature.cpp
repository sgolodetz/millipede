/***
 * millipede: AbdominalFeature.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "partitionforests/images/AbdominalFeature.h"

#include "exceptions/Exception.h"

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
	m.insert(std::make_pair(KIDNEY_LEFT,		make_rgba32(255,255,0,50)));
	m.insert(std::make_pair(KIDNEY_RIGHT,		make_rgba32(255,255,0,50)));
	m.insert(std::make_pair(LIVER,				make_rgba32(128,0,128,50)));
	m.insert(std::make_pair(OTHER_ARTERY,		make_rgba32(128,0,0,50)));
	m.insert(std::make_pair(OTHER_VEIN,			make_rgba32(0,0,128,50)));
	m.insert(std::make_pair(RIB,				make_rgba32(192,255,192,150)));
	m.insert(std::make_pair(SPINAL_CORD,		make_rgba32(128,255,255,150)));
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

std::string feature_to_name(AbdominalFeature::Enum e)
{
	using namespace AbdominalFeature;
	switch(e)
	{
		case AORTA:					return "Aorta";
		case INFERIOR_VENA_CAVA:	return "Inferior Vena Cava";
		case KIDNEY:				return "Kidney";
		case KIDNEY_LEFT:			return "Kidney (Left)";
		case KIDNEY_RIGHT:			return "Kidney (Right)";
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

template <> AbdominalFeature::Enum name_to_feature(const std::string& name)
{
	using namespace AbdominalFeature;

	if(false)								; // No-op - done this way for reasons of consistency
	else if(name == "Aorta")				return AORTA;
	else if(name == "Inferior Vena Cava")	return INFERIOR_VENA_CAVA;
	else if(name == "Kidney")				return KIDNEY;
	else if(name == "Kidney (Left)")		return KIDNEY_LEFT;
	else if(name == "Kidney (Right)")		return KIDNEY_RIGHT;
	else if(name == "Liver")				return LIVER;
	else if(name == "Other Artery")			return OTHER_ARTERY;
	else if(name == "Other Vein")			return OTHER_VEIN;
	else if(name == "Rib")					return RIB;
	else if(name == "Spinal Cord")			return SPINAL_CORD;
	else if(name == "Spleen")				return SPLEEN;
	else if(name == "Vertebra")				return VERTEBRA;
	else									throw Exception("Unknown feature name");
}

}
