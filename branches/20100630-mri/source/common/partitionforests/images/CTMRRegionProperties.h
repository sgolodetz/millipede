/***
 * millipede: CTMRRegionProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTMRREGIONPROPERTIES
#define H_MILLIPEDE_CTMRREGIONPROPERTIES

#include <vector>

#include "CTMRPixelProperties.h"

namespace mp {

class CTMRRegionProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	size_t m_area;
	double m_meanGreyValue;

	//#################### CONSTRUCTORS ####################
public:
	CTMRRegionProperties();

	//#################### PUBLIC METHODS ####################
public:
	int area() const;
	static CTMRRegionProperties combine_branch_properties(const std::vector<CTMRRegionProperties>& properties);
	static CTMRRegionProperties combine_leaf_properties(const std::vector<CTMRPixelProperties>& properties);
	double mean_grey_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const CTMRRegionProperties& rhs);

}

#endif
