/***
 * millipede: DICOMRegionProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "DICOMRegionProperties.h"

#include <ostream>
#include <sstream>

#include <boost/lexical_cast.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMRegionProperties::DICOMRegionProperties()
:	m_meanGreyValue(0.0),
	m_voxelCount(0)
{}

//#################### PUBLIC METHODS ####################
// Precondition: !properties.empty()
DICOMRegionProperties DICOMRegionProperties::combine_branch_properties(const std::vector<DICOMRegionProperties>& properties)
{
	DICOMRegionProperties ret;

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_voxelCount += properties[i].m_voxelCount;
		ret.m_meanGreyValue += properties[i].m_meanGreyValue * properties[i].m_voxelCount;
	}
	ret.m_meanGreyValue /= ret.m_voxelCount;

	return ret;
}

// Precondition: !properties.empty()
DICOMRegionProperties DICOMRegionProperties::combine_leaf_properties(const std::vector<DICOMPixelProperties>& properties)
{
	DICOMRegionProperties ret;

	ret.m_voxelCount = properties.size();

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

std::map<std::string,std::string> DICOMRegionProperties::property_map() const
{
	std::map<std::string,std::string> m;

	{
		std::ostringstream oss;
		oss.setf(std::ios::fixed, std::ios::floatfield);
		oss.precision(2);
		oss << m_meanGreyValue;
		m.insert(std::make_pair("Mean Grey Value", oss.str()));
	}

	m.insert(std::make_pair("Voxel Count", boost::lexical_cast<std::string>(m_voxelCount)));
	return m;
}

std::vector<std::string> DICOMRegionProperties::property_names()
{
	std::vector<std::string> names;
	names.push_back("Mean Grey Value");
	names.push_back("Voxel Count");
	return names;
}

int DICOMRegionProperties::voxel_count() const
{
	return m_voxelCount;
}

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const DICOMRegionProperties& rhs)
{
	os << "<<" << rhs.voxel_count() << " | " << rhs.mean_grey_value() << ">>";
	return os;
}

}
