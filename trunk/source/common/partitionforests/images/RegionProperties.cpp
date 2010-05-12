/***
 * millipede: RegionProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "RegionProperties.h"

#include <ostream>

namespace mp {

//#################### PUBLIC METHODS ####################
int RegionProperties::area() const
{
	return m_area;
}

// Precondition: !properties.empty()
RegionProperties RegionProperties::combine_branch_properties(const std::vector<RegionProperties>& properties)
{
	RegionProperties ret;

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_area += properties[i].m_area;
		ret.m_meanGreyValue += properties[i].m_meanGreyValue * properties[i].m_area;
	}
	ret.m_meanGreyValue /= ret.m_area;

	return ret;
}

// Precondition: !properties.empty()
RegionProperties RegionProperties::combine_leaf_properties(const std::vector<PixelProperties>& properties)
{
	RegionProperties ret;

	ret.m_area = properties.size();

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_meanGreyValue += properties[i].grey_value();
	}
	ret.m_meanGreyValue /= properties.size();

	return ret;
}

double RegionProperties::mean_grey_value() const
{
	return m_meanGreyValue;
}

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const RegionProperties& rhs)
{
	os << "<<" << rhs.area() << " | " << rhs.mean_grey_value() << ">>";
	return os;
}

}
