/***
 * millipede: MRPixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MRPixelProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
MRPixelProperties::MRPixelProperties()
:	m_gradientMagnitudeValue(0),
	m_greyValue(0)
{}

MRPixelProperties::MRPixelProperties(short gradientMagnitudeValue, unsigned char greyValue)
:	m_gradientMagnitudeValue(gradientMagnitudeValue),
	m_greyValue(greyValue)
{}

//#################### PUBLIC METHODS ####################
short MRPixelProperties::gradient_magnitude_value() const	{ return m_gradientMagnitudeValue; }
unsigned char MRPixelProperties::grey_value() const			{ return m_greyValue; }

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const MRPixelProperties& rhs)
{
	os << '<' << rhs.gradient_magnitude_value() << " | " << static_cast<int>(rhs.grey_value()) << '>';
	return os;
}

}
