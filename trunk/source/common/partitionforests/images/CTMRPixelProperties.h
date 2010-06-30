/***
 * millipede: CTMRPixelProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTMRPIXELPROPERTIES
#define H_MILLIPEDE_CTMRPIXELPROPERTIES

#include <iosfwd>

namespace mp {

class CTMRPixelProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	// Note: Deliberately listed in non-increasing order of data size to try and minimize padding.
	int m_baseValue;
	short m_gradientMagnitudeValue;
	unsigned char m_greyValue;

	//#################### CONSTRUCTORS ####################
public:
	CTMRPixelProperties();
	CTMRPixelProperties(int baseValue, short gradientMagnitudeValue, unsigned char greyValue);

	//#################### PUBLIC METHODS ####################
public:
	int base_value() const;
	short gradient_magnitude_value() const;
	unsigned char grey_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTMRPixelProperties& rhs);

}

#endif
