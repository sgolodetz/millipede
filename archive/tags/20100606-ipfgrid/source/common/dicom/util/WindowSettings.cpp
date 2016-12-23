/***
 * millipede: WindowSettings.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "WindowSettings.h"

namespace mp {

//#################### CONSTRUCTORS ####################
WindowSettings::WindowSettings()
:	m_centre(0), m_width(0)
{}

WindowSettings::WindowSettings(double centre, double width)
:	m_centre(centre), m_width(width)
{}

//#################### PUBLIC METHODS ####################
double WindowSettings::centre() const		{ return m_centre; }
bool WindowSettings::unspecified() const	{ return m_centre == 0 && m_width == 0; }
double WindowSettings::width() const		{ return m_width; }

}
