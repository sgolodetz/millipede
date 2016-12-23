/***
 * millipede: CTPixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTPixelProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
CTPixelProperties::CTPixelProperties(short gradientMagnitudeValue, unsigned char greyValue, int hounsfieldValue)
:	m_hounsfieldValue(hounsfieldValue), m_gradientMagnitudeValue(gradientMagnitudeValue), m_greyValue(greyValue)
{}

//#################### PUBLIC METHODS ####################
short CTPixelProperties::gradient_magnitude_value() const	{ return m_gradientMagnitudeValue; }
unsigned char CTPixelProperties::grey_value() const			{ return m_greyValue; }
int CTPixelProperties::hounsfield_value() const				{ return m_hounsfieldValue; }

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTPixelProperties& rhs)
{
	os << '<' << rhs.gradient_magnitude_value() << " | " << static_cast<int>(rhs.grey_value()) << " | " << rhs.hounsfield_value() << '>';
	return os;
}

}
