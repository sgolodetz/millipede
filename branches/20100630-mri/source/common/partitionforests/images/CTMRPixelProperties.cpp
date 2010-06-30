/***
 * millipede: CTMRPixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTMRPixelProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
CTMRPixelProperties::CTMRPixelProperties()
:	m_hounsfieldValue(0),
	m_gradientMagnitudeValue(0),
	m_greyValue(0)
{}

CTMRPixelProperties::CTMRPixelProperties(short gradientMagnitudeValue, unsigned char greyValue, int hounsfieldValue)
:	m_hounsfieldValue(hounsfieldValue),
	m_gradientMagnitudeValue(gradientMagnitudeValue),
	m_greyValue(greyValue)
{}

//#################### PUBLIC METHODS ####################
short CTMRPixelProperties::gradient_magnitude_value() const		{ return m_gradientMagnitudeValue; }
unsigned char CTMRPixelProperties::grey_value() const			{ return m_greyValue; }
int CTMRPixelProperties::hounsfield_value() const				{ return m_hounsfieldValue; }

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTMRPixelProperties& rhs)
{
	os << '<' << rhs.gradient_magnitude_value() << " | " << static_cast<int>(rhs.grey_value()) << " | " << rhs.hounsfield_value() << '>';
	return os;
}

}
