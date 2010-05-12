/***
 * millipede: PixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "PixelProperties.h"

namespace mp {

//#################### CONSTRUCTORS ####################
PixelProperties::PixelProperties(unsigned char greyValue, signed int hounsfieldValue)
:	m_greyValue(greyValue), m_hounsfieldValue(hounsfieldValue)
{}

//#################### PUBLIC METHODS ####################
unsigned char PixelProperties::grey_value() const		{ return m_greyValue; }
signed int PixelProperties::hounsfield_value() const	{ return m_hounsfieldValue; }

}
