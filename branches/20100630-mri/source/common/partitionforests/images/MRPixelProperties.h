/***
 * millipede: MRPixelProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MRPIXELPROPERTIES
#define H_MILLIPEDE_MRPIXELPROPERTIES

#include <iosfwd>

namespace mp {

class MRPixelProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	// Note: Deliberately listed in non-increasing order of data size to try and minimize padding.
	short m_gradientMagnitudeValue;
	unsigned char m_greyValue;

	//#################### CONSTRUCTORS ####################
public:
	MRPixelProperties();
	MRPixelProperties(short gradientMagnitudeValue, unsigned char greyValue);

	//#################### PUBLIC METHODS ####################
public:
	short gradient_magnitude_value() const;
	unsigned char grey_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const MRPixelProperties& rhs);

}

#endif
