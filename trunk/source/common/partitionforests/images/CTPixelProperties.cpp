/***
 * millipede: CTPixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTPixelProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
CTPixelProperties::CTPixelProperties(unsigned char greyValue, int hounsfieldValue)
:	m_greyValue(greyValue), m_hounsfieldValue(hounsfieldValue)
{}

//#################### PUBLIC METHODS ####################
unsigned char CTPixelProperties::grey_value() const		{ return m_greyValue; }
int CTPixelProperties::hounsfield_value() const			{ return m_hounsfieldValue; }

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTPixelProperties& rhs)
{
	os << '<' << static_cast<int>(rhs.grey_value()) << " | " << rhs.hounsfield_value() << '>';
	return os;
}

}
