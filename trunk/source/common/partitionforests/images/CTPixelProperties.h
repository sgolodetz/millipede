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
	unsigned char m_greyValue;
	int m_hounsfieldValue;

	//#################### CONSTRUCTORS ####################
public:
	CTPixelProperties(unsigned char greyValue, int hounsfieldValue);

	//#################### PUBLIC METHODS ####################
public:
	unsigned char grey_value() const;
	int hounsfield_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTPixelProperties& rhs);

}

#endif
