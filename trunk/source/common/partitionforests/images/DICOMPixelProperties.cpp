/***
 * millipede: DICOMPixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 * Modified by Varduhi Yeghiazaryan, 2013.
 ***/

#include "DICOMPixelProperties.h"

#include <ostream>

#include <boost/lexical_cast.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMPixelProperties::DICOMPixelProperties()
:	m_baseValue(0),
	m_gradientMagnitudeValue(0),
	m_greyValue(0)
{}

DICOMPixelProperties::DICOMPixelProperties(int baseValue, short gradientMagnitudeValue, unsigned char greyValue)
:	m_baseValue(baseValue),
	m_gradientMagnitudeValue(gradientMagnitudeValue),
	m_greyValue(greyValue)
{}

//#################### PUBLIC METHODS ####################
int DICOMPixelProperties::base_value() const
{
	return m_baseValue;
}

std::map<std::string,std::string> DICOMPixelProperties::branch_property_map() const
{
	std::map<std::string,std::string> m;
	m.insert(std::make_pair("Mean Grey Value", boost::lexical_cast<std::string,int>(m_greyValue)));
	m.insert(std::make_pair("Voxel Count", "1"));
	return m;
}

short DICOMPixelProperties::gradient_magnitude_value() const
{
	return m_gradientMagnitudeValue;
}

unsigned char DICOMPixelProperties::grey_value() const
{
	return m_greyValue;
}

//#################### GLOBAL OPERATORS ####################
std::istream& operator>>(std::istream& is, DICOMPixelProperties& rhs)
{
	char dummy;
	int baseValue, greyValue;
	short gradientMagnitudeValue;
	is >> dummy >> baseValue >> dummy >> gradientMagnitudeValue >> dummy >> greyValue >> dummy;
	rhs.m_baseValue = baseValue;
	rhs.m_gradientMagnitudeValue = gradientMagnitudeValue;
	rhs.m_greyValue = static_cast<unsigned char>(greyValue);
	return is;
}

std::ostream& operator<<(std::ostream& os, const DICOMPixelProperties& rhs)
{
	// The whitespaces were removed from the output string!
	os << '<' << rhs.base_value() << '|' << rhs.gradient_magnitude_value() << '|' << static_cast<int>(rhs.grey_value()) << '>';
	return os;
}

}
