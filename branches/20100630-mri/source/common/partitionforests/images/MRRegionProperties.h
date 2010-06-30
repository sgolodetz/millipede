/***
 * millipede: MRRegionProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MRREGIONPROPERTIES
#define H_MILLIPEDE_MRREGIONPROPERTIES

#include <vector>

#include "MRPixelProperties.h"

namespace mp {

// Note:	This is currently very similar to CTRegionProperties, but I'm not yet sure whether the two should be different or not (hence I'm leaving it for now).
//			If they remain the same, the common functionality should ultimately be factored out.
class MRRegionProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	size_t m_area;
	double m_meanGreyValue;

	//#################### CONSTRUCTORS ####################
public:
	MRRegionProperties();

	//#################### PUBLIC METHODS ####################
public:
	int area() const;
	static MRRegionProperties combine_branch_properties(const std::vector<MRRegionProperties>& properties);
	static MRRegionProperties combine_leaf_properties(const std::vector<MRPixelProperties>& properties);
	double mean_grey_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const MRRegionProperties& rhs);

}

#endif
