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
	int m_greyValue;

	//#################### CONSTRUCTORS ####################
public:
	PixelProperties(int greyValue);

	//#################### PUBLIC METHODS ####################
public:
	int grey_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const PixelProperties& rhs);

}

#endif
