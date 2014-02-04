/***
 * millipede: DICOMRegionProperties.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "DICOMRegionProperties.h"

#include <climits>
#include <ostream>
#include <sstream>
#include <iostream>

#include <boost/lexical_cast.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMRegionProperties::DICOMRegionProperties()
:	m_centroid(0.0, 0.0, 0.0),
	m_maxGreyValue(0),
	m_meanGreyValue(0.0),
	m_minGreyValue(UCHAR_MAX),
	m_xMin(INT_MAX), m_yMin(INT_MAX), m_zMin(INT_MAX), m_xMax(INT_MIN), m_yMax(INT_MIN), m_zMax(INT_MIN),
	m_voxelCount(0), 
	m_pointInside(0.0, 0.0, 0.0)
{
	
	m_voxPerSlice = 0;
	
}

//#################### PUBLIC METHODS ####################
double DICOMRegionProperties::aspect_ratio_xy() const
{
	return static_cast<double>(m_xMax - m_xMin) / (m_yMax - m_yMin);
}

std::map<std::string,std::string> DICOMRegionProperties::branch_property_map() const
{
	std::map<std::string,std::string> m;

	m.insert(std::make_pair("Centroid", boost::lexical_cast<std::string>(m_centroid)));
	m.insert(std::make_pair("Max Grey Value", boost::lexical_cast<std::string,int>(m_maxGreyValue)));

	{
		std::ostringstream oss;
		oss.setf(std::ios::fixed, std::ios::floatfield);
		oss.precision(2);
		oss << m_meanGreyValue;
		m.insert(std::make_pair("Mean Grey Value", oss.str()));
	}
	
	{
		std::ostringstream oss;
		oss.setf(std::ios::fixed, std::ios::floatfield);
		oss.precision(2);
		oss << m_meanHoundsfieldValue;
		m.insert(std::make_pair("Mean Houndsfield Value", oss.str()));
		//std::cout << "DICOMRegionProperties.cpp - branch_property_map " << oss.str() <<std::endl;
	}

	m.insert(std::make_pair("Min Grey Value", boost::lexical_cast<std::string,int>(m_minGreyValue)));
	m.insert(std::make_pair("Voxel Count", boost::lexical_cast<std::string>(m_voxelCount)));
	m.insert(std::make_pair("X Min", boost::lexical_cast<std::string>(m_xMin)));
	m.insert(std::make_pair("Y Min", boost::lexical_cast<std::string>(m_yMin)));
	m.insert(std::make_pair("Z Min", boost::lexical_cast<std::string>(m_zMin)));
	m.insert(std::make_pair("X Max", boost::lexical_cast<std::string>(m_xMax)));
	m.insert(std::make_pair("Voxels Per Slice", boost::lexical_cast<std::string>(m_voxPerSlice)));
	m.insert(std::make_pair("Y Max", boost::lexical_cast<std::string>(m_yMax)));
	m.insert(std::make_pair("Z Max", boost::lexical_cast<std::string>(m_zMax)));
	return m;
}

std::vector<std::string> DICOMRegionProperties::branch_property_names()
{
	std::vector<std::string> names;
	names.push_back("Centroid");
	names.push_back("Mean Houndsfield Value");
	names.push_back("Voxels Per Slice");
	names.push_back("Mean Grey Value");
	names.push_back("Min Grey Value");
	names.push_back("Max Grey Value");
	names.push_back("Voxel Count");
	names.push_back("X Min");
	names.push_back("Y Min");
	names.push_back("Z Min");
	names.push_back("X Max");
	names.push_back("Y Max");
	names.push_back("Z Max");
	return names;
}

const Vector3d& DICOMRegionProperties::centroid() const
{
	return m_centroid;
}

const Vector3i& DICOMRegionProperties::point_inside() const
{
	return m_pointInside;
}

int DICOMRegionProperties::voxels_per_slice() const {
	
	return voxel_count() / (z_max() + 1 - z_min());
}
	

// Precondition: !properties.empty()
DICOMRegionProperties DICOMRegionProperties::combine_branch_properties(const std::vector<DICOMRegionProperties>& properties)
{
	DICOMRegionProperties ret;

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_centroid += properties[i].m_centroid * properties[i].m_voxelCount;
		ret.m_maxGreyValue = std::max(ret.m_maxGreyValue, properties[i].m_maxGreyValue);
		ret.m_meanGreyValue += properties[i].m_meanGreyValue * properties[i].m_voxelCount;
		ret.m_meanHoundsfieldValue += properties[i].m_meanHoundsfieldValue * properties[i].m_voxelCount;
		ret.m_minGreyValue = std::min(ret.m_minGreyValue, properties[i].m_minGreyValue);
		ret.m_voxelCount += properties[i].m_voxelCount;

		ret.m_xMin = std::min(ret.m_xMin, properties[i].m_xMin);	ret.m_xMax = std::max(ret.m_xMax, properties[i].m_xMax);
		ret.m_yMin = std::min(ret.m_yMin, properties[i].m_yMin);	ret.m_yMax = std::max(ret.m_yMax, properties[i].m_yMax);
		ret.m_zMin = std::min(ret.m_zMin, properties[i].m_zMin);	ret.m_zMax = std::max(ret.m_zMax, properties[i].m_zMax);
	}
	ret.m_centroid /= ret.m_voxelCount;
	
	if (ret.m_voxelCount != 0) {
		ret.m_meanGreyValue /= ret.m_voxelCount;
		//std::cout << "DICOMRegionProperties.cpp - combine_branch_properties a mhv " << ret.m_meanHoundsfieldValue << std::endl;
		//std::cout << "DICOMRegionProperties.cpp - combine_branch_properties a vox " << ret.m_voxelCount << std::endl;
		ret.m_meanHoundsfieldValue /= ret.m_voxelCount;
		//std::cout << "DICOMRegionProperties.cpp - combine_branch_properties b mhv " << ret.m_meanHoundsfieldValue << std::endl;
		//std::cout << "DICOMRegionProperties.cpp - combine_branch_properties b vox " << ret.m_voxelCount << std::endl;
		ret.m_voxPerSlice = ret.voxels_per_slice();
	}
		
	ret.m_pointInside = properties[0].point_inside();
	
	return ret;
}

// Precondition: !properties.empty()
DICOMRegionProperties DICOMRegionProperties::combine_leaf_properties(const std::vector<std::pair<Vector3i,DICOMPixelProperties> >& properties)
{
	DICOMRegionProperties ret;

	ret.m_voxelCount = properties.size();

	for(size_t i=0, size=properties.size(); i<size; ++i)
	{
		ret.m_centroid += Vector3d(properties[i].first);
		ret.m_maxGreyValue = std::max(ret.m_maxGreyValue, properties[i].second.grey_value());
		ret.m_meanGreyValue += properties[i].second.grey_value();
		ret.m_meanHoundsfieldValue += properties[i].second.base_value();
		ret.m_minGreyValue = std::min(ret.m_minGreyValue, properties[i].second.grey_value());

		int x = properties[i].first.x, y = properties[i].first.y, z = properties[i].first.z;
		ret.m_xMin = std::min(ret.m_xMin, x);	ret.m_xMax = std::max(ret.m_xMax, x);
		ret.m_yMin = std::min(ret.m_yMin, y);	ret.m_yMax = std::max(ret.m_yMax, y);
		ret.m_zMin = std::min(ret.m_zMin, z);	ret.m_zMax = std::max(ret.m_zMax, z);
	}
	ret.m_centroid /= ret.m_voxelCount;
	ret.m_meanGreyValue /= ret.m_voxelCount;
	ret.m_meanHoundsfieldValue /= ret.m_voxelCount;
	ret.m_voxPerSlice = ret.voxels_per_slice();
	//std::cout << "DICOMRegionProperties.cpp - combine_leaf_properties " << ret.m_meanHoundsfieldValue <<std::endl;
	
	ret.m_pointInside = properties[0].first;

	return ret;
}

DICOMRegionProperties DICOMRegionProperties::convert_from_leaf_properties(const std::pair<Vector3i,DICOMPixelProperties>& properties)
{
	DICOMRegionProperties ret;
	ret.m_centroid = Vector3d(properties.first);
	ret.m_maxGreyValue = properties.second.grey_value();
	ret.m_meanGreyValue = properties.second.grey_value();
	ret.m_meanHoundsfieldValue = properties.second.base_value();
	ret.m_minGreyValue = properties.second.grey_value();
	ret.m_voxelCount = 1;
	ret.m_xMin = ret.m_xMax = properties.first.x;
	ret.m_yMin = ret.m_yMax = properties.first.y;
	ret.m_zMin = ret.m_zMax = properties.first.z;
	//std::cout << "DICOMRegionProperties.cpp - convert_from_leaf_properties " << ret.m_meanHoundsfieldValue <<std::endl;
	
	ret.m_pointInside = Vector3i(properties.first);
	ret.m_voxPerSlice = ret.voxels_per_slice();
	
	return ret;
}

unsigned char DICOMRegionProperties::max_grey_value() const		{ return m_maxGreyValue; }
double DICOMRegionProperties::mean_grey_value() const			{ return m_meanGreyValue; }
double DICOMRegionProperties::mean_houndsfield_value() const			{ return m_meanHoundsfieldValue; }
unsigned char DICOMRegionProperties::min_grey_value() const		{ return m_minGreyValue; }
int DICOMRegionProperties::voxel_count() const					{ return m_voxelCount; }
int DICOMRegionProperties::x_max() const						{ return m_xMax; }
int DICOMRegionProperties::x_min() const						{ return m_xMin; }
int DICOMRegionProperties::y_max() const						{ return m_yMax; }
int DICOMRegionProperties::y_min() const						{ return m_yMin; }
int DICOMRegionProperties::z_max() const						{ return m_zMax; }
int DICOMRegionProperties::z_min() const						{ return m_zMin; }

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const DICOMRegionProperties& rhs)
{
	os << "<<" << rhs.voxel_count() << " | " << rhs.mean_grey_value() << ">>";
	return os;
}

}
