/***
 * millipede: CTRegionProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTREGIONPROPERTIES
#define H_MILLIPEDE_CTREGIONPROPERTIES

#include <vector>

#include "CTPixelProperties.h"

namespace mp {

// Note:	This is currently very similar to MRRegionProperties, but I'm not yet sure whether the two should be different or not (hence I'm leaving it for now).
//			If they remain the same, the common functionality should ultimately be factored out.
class CTRegionProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	size_t m_area;
	double m_meanGreyValue;

	//#################### CONSTRUCTORS ####################
public:
	CTRegionProperties();

	//#################### PUBLIC METHODS ####################
public:
	int area() const;
	static CTRegionProperties combine_branch_properties(const std::vector<CTRegionProperties>& properties);
	static CTRegionProperties combine_leaf_properties(const std::vector<CTPixelProperties>& properties);
	double mean_grey_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTRegionProperties& rhs);

}

#endif
