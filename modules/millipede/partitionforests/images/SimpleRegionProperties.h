/***
 * millipede: SimpleRegionProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SIMPLEREGIONPROPERTIES
#define H_MILLIPEDE_SIMPLEREGIONPROPERTIES

#include <vector>

#include <millipede/math/Vector3.h>
#include "SimplePixelProperties.h"

namespace mp {

class SimpleRegionProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_area;
	double m_meanValue;

	//#################### CONSTRUCTORS ####################
public:
	SimpleRegionProperties();

	//#################### PUBLIC METHODS ####################
public:
	int area() const;
	static SimpleRegionProperties combine_branch_properties(const std::vector<SimpleRegionProperties>& properties);
	static SimpleRegionProperties combine_leaf_properties(const std::vector<std::pair<Vector3i,SimplePixelProperties> >& properties);
	double mean_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const SimpleRegionProperties& rhs);

}

#endif
