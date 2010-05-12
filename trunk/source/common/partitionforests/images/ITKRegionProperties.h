/***
 * millipede: ITKRegionProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ITKREGIONPROPERTIES
#define H_MILLIPEDE_ITKREGIONPROPERTIES

#include <iosfwd>
#include <vector>

#include "ITKPixelProperties.h"

namespace mp {

class ITKRegionProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	size_t m_area;
	double m_meanGreyValue;

	//#################### CONSTRUCTORS ####################
public:
	ITKRegionProperties()
	:	m_area(0),
		m_meanGreyValue(0.0)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int area() const;
	static ITKRegionProperties combine_branch_properties(const std::vector<ITKRegionProperties>& properties);
	static ITKRegionProperties combine_leaf_properties(const std::vector<ITKPixelProperties>& properties);
	double mean_grey_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const ITKRegionProperties& rhs);

}

#endif
