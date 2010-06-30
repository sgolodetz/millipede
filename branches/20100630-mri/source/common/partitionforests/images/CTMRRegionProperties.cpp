/***
 * millipede: CTMRRegionProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTMRRegionProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
CTMRRegionProperties::CTMRRegionProperties()
:	m_area(0),
	m_meanGreyValue(0.0)
{}

//#################### PUBLIC METHODS ####################
int CTMRRegionProperties::area() const
{
	return m_area;
}

// Precondition: !properties.empty()
CTMRRegionProperties CTMRRegionProperties::combine_branch_properties(const std::vector<CTMRRegionProperties>& properties)
{
	CTMRRegionProperties ret;

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_area += properties[i].m_area;
		ret.m_meanGreyValue += properties[i].m_meanGreyValue * properties[i].m_area;
	}
	ret.m_meanGreyValue /= ret.m_area;

	return ret;
}

// Precondition: !properties.empty()
CTMRRegionProperties CTMRRegionProperties::combine_leaf_properties(const std::vector<CTMRPixelProperties>& properties)
{
	CTMRRegionProperties ret;

	ret.m_area = properties.size();

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_meanGreyValue += properties[i].grey_value();
	}
	ret.m_meanGreyValue /= properties.size();

	return ret;
}

double CTMRRegionProperties::mean_grey_value() const
{
	return m_meanGreyValue;
}

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTMRRegionProperties& rhs)
{
	os << "<<" << rhs.area() << " | " << rhs.mean_grey_value() << ">>";
	return os;
}

}
