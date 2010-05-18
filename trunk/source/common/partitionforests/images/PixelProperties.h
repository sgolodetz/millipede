/***
 * millipede: PixelProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PIXELPROPERTIES
#define H_MILLIPEDE_PIXELPROPERTIES

#include <iosfwd>

namespace mp {

class PixelProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	unsigned char m_greyValue;
	int m_hounsfieldValue;

	//#################### CONSTRUCTORS ####################
public:
	PixelProperties(unsigned char greyValue, int hounsfieldValue);

	//#################### PUBLIC METHODS ####################
public:
	unsigned char grey_value() const;
	int hounsfield_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const PixelProperties& rhs);

}

#endif
