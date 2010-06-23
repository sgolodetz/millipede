/***
 * millipede: CTRegionProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTRegionProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
CTRegionProperties::CTRegionProperties()
:	m_area(0),
	m_meanGreyValue(0.0)
{}

//#################### PUBLIC METHODS ####################
int CTRegionProperties::area() const
{
	return m_area;
}

// Precondition: !properties.empty()
CTRegionProperties CTRegionProperties::combine_branch_properties(const std::vector<CTRegionProperties>& properties)
{
	CTRegionProperties ret;

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_area += properties[i].m_area;
		ret.m_meanGreyValue += properties[i].m_meanGreyValue * properties[i].m_area;
	}
	ret.m_meanGreyValue /= ret.m_area;

	return ret;
}

// Precondition: !properties.empty()
CTRegionProperties CTRegionProperties::combine_leaf_properties(const std::vector<CTPixelProperties>& properties)
{
	CTRegionProperties ret;

	ret.m_area = properties.size();

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_meanGreyValue += properties[i].grey_value();
	}
	ret.m_meanGreyValue /= properties.size();

	return ret;
}

double CTRegionProperties::mean_grey_value() const
{
	return m_meanGreyValue;
}

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTRegionProperties& rhs)
{
	os << "<<" << rhs.area() << " | " << rhs.mean_grey_value() << ">>";
	return os;
}

}
