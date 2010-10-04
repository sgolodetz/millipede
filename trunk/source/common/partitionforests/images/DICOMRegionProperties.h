/***
 * millipede: DICOMRegionProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMREGIONPROPERTIES
#define H_MILLIPEDE_DICOMREGIONPROPERTIES

#include <map>
#include <string>
#include <vector>

#include <common/math/Vector3.h>
#include "DICOMPixelProperties.h"

namespace mp {

class DICOMRegionProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	Vector3d m_centroid;
	unsigned char m_maxGreyValue;
	double m_meanGreyValue;
	unsigned char m_minGreyValue;
	int m_xMin, m_yMin, m_zMin, m_xMax, m_yMax, m_zMax;
	size_t m_voxelCount;

	//#################### CONSTRUCTORS ####################
public:
	DICOMRegionProperties();

	//#################### PUBLIC METHODS ####################
public:
	double aspect_ratio_xy() const;
	std::map<std::string,std::string> branch_property_map() const;
	static std::vector<std::string> branch_property_names();
	const Vector3d& centroid() const;
	static DICOMRegionProperties combine_branch_properties(const std::vector<DICOMRegionProperties>& properties);
	static DICOMRegionProperties combine_leaf_properties(const std::vector<std::pair<Vector3i,DICOMPixelProperties> >& properties);
	static DICOMRegionProperties convert_from_leaf_properties(const std::pair<Vector3i,DICOMPixelProperties>& properties);
	unsigned char max_grey_value() const;
	double mean_grey_value() const;
	unsigned char min_grey_value() const;
	int voxel_count() const;
	int x_max() const;
	int x_min() const;
	int y_max() const;
	int y_min() const;
	int z_max() const;
	int z_min() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const DICOMRegionProperties& rhs);

}

#endif
