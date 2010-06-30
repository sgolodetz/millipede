/***
 * millipede: CTMRPixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTMRPixelProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
CTMRPixelProperties::CTMRPixelProperties()
:	m_baseValue(0),
	m_gradientMagnitudeValue(0),
	m_greyValue(0)
{}

CTMRPixelProperties::CTMRPixelProperties(int baseValue, short gradientMagnitudeValue, unsigned char greyValue)
:	m_baseValue(baseValue),
	m_gradientMagnitudeValue(gradientMagnitudeValue),
	m_greyValue(greyValue)
{}

//#################### PUBLIC METHODS ####################
int CTMRPixelProperties::base_value() const						{ return m_baseValue; }
short CTMRPixelProperties::gradient_magnitude_value() const		{ return m_gradientMagnitudeValue; }
unsigned char CTMRPixelProperties::grey_value() const			{ return m_greyValue; }

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTMRPixelProperties& rhs)
{
	os << '<' << rhs.base_value() << " | " << rhs.gradient_magnitude_value() << " | " << static_cast<int>(rhs.grey_value()) << '>';
	return os;
}

}
