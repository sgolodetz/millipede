/***
 * millipede: PixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "PixelProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
PixelProperties::PixelProperties(unsigned char greyValue, int hounsfieldValue)
:	m_greyValue(greyValue), m_hounsfieldValue(hounsfieldValue)
{}

//#################### PUBLIC METHODS ####################
unsigned char PixelProperties::grey_value() const	{ return m_greyValue; }
int PixelProperties::hounsfield_value() const		{ return m_hounsfieldValue; }

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const PixelProperties& rhs)
{
	os << '<' << static_cast<int>(rhs.grey_value()) << " | " << rhs.hounsfield_value() << '>';
	return os;
}

}
