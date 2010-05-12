/***
 * millipede: ITKPixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "ITKPixelProperties.h"

namespace mp {

//#################### CONSTRUCTORS ####################
ITKPixelProperties::ITKPixelProperties(unsigned char greyValue, signed int hounsfieldValue)
:	m_greyValue(greyValue), m_hounsfieldValue(hounsfieldValue)
{}

//#################### PUBLIC METHODS ####################
unsigned char ITKPixelProperties::grey_value() const		{ return m_greyValue; }
signed int ITKPixelProperties::hounsfield_value() const		{ return m_hounsfieldValue; }

}
