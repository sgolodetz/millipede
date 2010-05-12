/***
 * millipede: RegionProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_REGIONPROPERTIES
#define H_MILLIPEDE_REGIONPROPERTIES

#include <iosfwd>
#include <vector>

#include "PixelProperties.h"

namespace mp {

class RegionProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	size_t m_area;
	double m_meanGreyValue;

	//#################### CONSTRUCTORS ####################
public:
	RegionProperties()
	:	m_area(0),
		m_meanGreyValue(0.0)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int area() const;
	static RegionProperties combine_branch_properties(const std::vector<RegionProperties>& properties);
	static RegionProperties combine_leaf_properties(const std::vector<PixelProperties>& properties);
	double mean_grey_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const RegionProperties& rhs);

}

#endif
