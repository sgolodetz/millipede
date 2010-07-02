/***
 * millipede: SimpleRegionProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SimpleRegionProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
SimpleRegionProperties::SimpleRegionProperties()
:	m_area(0),
	m_meanValue(0.0)
{}

//#################### PUBLIC METHODS ####################
int SimpleRegionProperties::area() const
{
	return m_area;
}

// Precondition: !properties.empty()
SimpleRegionProperties SimpleRegionProperties::combine_branch_properties(const std::vector<SimpleRegionProperties>& properties)
{
	SimpleRegionProperties ret;

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_area += properties[i].m_area;
		ret.m_meanValue += properties[i].m_meanValue * properties[i].m_area;
	}
	ret.m_meanValue /= ret.m_area;

	return ret;
}

// Precondition: !properties.empty()
SimpleRegionProperties SimpleRegionProperties::combine_leaf_properties(const std::vector<SimplePixelProperties>& properties)
{
	SimpleRegionProperties ret;

	ret.m_area = properties.size();

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_meanValue += properties[i].value();
	}
	ret.m_meanValue /= properties.size();

	return ret;
}

double SimpleRegionProperties::mean_value() const
{
	return m_meanValue;
}

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const SimpleRegionProperties& rhs)
{
	os << "<<" << rhs.area() << " | " << rhs.mean_value() << ">>";
	return os;
}

}
