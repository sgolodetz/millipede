/***
 * millipede: PixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "PixelProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
PixelProperties::PixelProperties(int greyValue)
:	m_greyValue(greyValue)
{}

//#################### PUBLIC METHODS ####################
int PixelProperties::grey_value() const	{ return m_greyValue; }

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const PixelProperties& rhs)
{
	os << '<' << rhs.grey_value() << '>';
	return os;
}

}
