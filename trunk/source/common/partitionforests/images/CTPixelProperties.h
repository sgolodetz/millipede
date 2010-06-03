/***
 * millipede: CTPixelProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTPIXELPROPERTIES
#define H_MILLIPEDE_CTPIXELPROPERTIES

#include <iosfwd>

namespace mp {

class CTPixelProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	// Note: Deliberately listed in non-increasing order of data size to try and minimize padding.
	int m_hounsfieldValue;
	short m_gradientMagnitudeValue;
	unsigned char m_greyValue;

	//#################### CONSTRUCTORS ####################
public:
	CTPixelProperties(short gradientMagnitudeValue, unsigned char greyValue, int hounsfieldValue);

	//#################### PUBLIC METHODS ####################
public:
	short gradient_magnitude_value() const;
	unsigned char grey_value() const;
	int hounsfield_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTPixelProperties& rhs);

}

#endif
