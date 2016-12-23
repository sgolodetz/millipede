/***
 * millipede: DICOMPixelProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "partitionforests/images/DICOMPixelProperties.h"

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
std::ostream& operator<<(std::ostream& os, const DICOMPixelProperties& rhs)
{
	os << '<' << rhs.base_value() << " | " << rhs.gradient_magnitude_value() << " | " << static_cast<int>(rhs.grey_value()) << '>';
	return os;
}

}
