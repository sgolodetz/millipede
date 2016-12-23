/***
 * millipede: SimplePixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SimplePixelProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
SimplePixelProperties::SimplePixelProperties(int value)
:	m_value(value)
{}

//#################### PUBLIC METHODS ####################
int SimplePixelProperties::value() const
{
	return m_value;
}

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const SimplePixelProperties& rhs)
{
	os << '<' << rhs.value() << '>';
	return os;
}

}
