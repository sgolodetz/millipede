/***
 * millipede: DICOMRegionProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "DICOMRegionProperties.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMRegionProperties::DICOMRegionProperties()
:	m_area(0),
	m_meanGreyValue(0.0)
{}

//#################### PUBLIC METHODS ####################
int DICOMRegionProperties::area() const
{
	return m_area;
}

// Precondition: !properties.empty()
DICOMRegionProperties DICOMRegionProperties::combine_branch_properties(const std::vector<DICOMRegionProperties>& properties)
{
	DICOMRegionProperties ret;

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_area += properties[i].m_area;
		ret.m_meanGreyValue += properties[i].m_meanGreyValue * properties[i].m_area;
	}
	ret.m_meanGreyValue /= ret.m_area;

	return ret;
}

// Precondition: !properties.empty()
DICOMRegionProperties DICOMRegionProperties::combine_leaf_properties(const std::vector<DICOMPixelProperties>& properties)
{
	DICOMRegionProperties ret;

	ret.m_area = properties.size();

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_meanGreyValue += properties[i].grey_value();
	}
	ret.m_meanGreyValue /= properties.size();

	return ret;
}

double DICOMRegionProperties::mean_grey_value() const
{
	return m_meanGreyValue;
}

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const DICOMRegionProperties& rhs)
{
	os << "<<" << rhs.area() << " | " << rhs.mean_grey_value() << ">>";
	return os;
}

}
