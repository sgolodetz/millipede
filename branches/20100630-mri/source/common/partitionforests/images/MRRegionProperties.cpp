/***
 * millipede: MRRegionProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MRRegionProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
MRRegionProperties::MRRegionProperties()
:	m_area(0),
	m_meanGreyValue(0.0)
{}

//#################### PUBLIC METHODS ####################
int MRRegionProperties::area() const
{
	return m_area;
}

// Precondition: !properties.empty()
MRRegionProperties MRRegionProperties::combine_branch_properties(const std::vector<MRRegionProperties>& properties)
{
	MRRegionProperties ret;

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_area += properties[i].m_area;
		ret.m_meanGreyValue += properties[i].m_meanGreyValue * properties[i].m_area;
	}
	ret.m_meanGreyValue /= ret.m_area;

	return ret;
}

// Precondition: !properties.empty()
MRRegionProperties MRRegionProperties::combine_leaf_properties(const std::vector<MRPixelProperties>& properties)
{
	MRRegionProperties ret;

	ret.m_area = properties.size();

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_meanGreyValue += properties[i].grey_value();
	}
	ret.m_meanGreyValue /= properties.size();

	return ret;
}

double MRRegionProperties::mean_grey_value() const
{
	return m_meanGreyValue;
}

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const MRRegionProperties& rhs)
{
	os << "<<" << rhs.area() << " | " << rhs.mean_grey_value() << ">>";
	return os;
}

}
