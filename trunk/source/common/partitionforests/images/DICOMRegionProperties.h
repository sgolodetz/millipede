/***
 * millipede: DICOMRegionProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMREGIONPROPERTIES
#define H_MILLIPEDE_DICOMREGIONPROPERTIES

#include <map>
#include <string>
#include <vector>

#include "DICOMPixelProperties.h"

namespace mp {

class DICOMRegionProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	double m_meanGreyValue;
	size_t m_voxelCount;

	//#################### CONSTRUCTORS ####################
public:
	DICOMRegionProperties();

	//#################### PUBLIC METHODS ####################
public:
	static DICOMRegionProperties combine_branch_properties(const std::vector<DICOMRegionProperties>& properties);
	static DICOMRegionProperties combine_leaf_properties(const std::vector<DICOMPixelProperties>& properties);
	double mean_grey_value() const;
	std::map<std::string,std::string> property_map() const;
	static std::vector<std::string> property_names();
	int voxel_count() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const DICOMRegionProperties& rhs);

}

#endif
