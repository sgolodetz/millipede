/***
 * millipede: TestImageNodeProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TESTIMAGENODEPROPERTIES
#define H_MILLIPEDE_TESTIMAGENODEPROPERTIES

namespace mp {

typedef int TestPixelProperties;

struct TestRegionProperties
{
	//#################### PUBLIC VARIABLES ####################
	int sum;

	//#################### CONSTRUCTORS ####################
	TestRegionProperties()
	:	sum(0)
	{}

	//#################### PUBLIC METHODS ####################
	static TestRegionProperties combine_branch_properties(const std::vector<TestRegionProperties>& properties)
	{
		TestRegionProperties ret;
		for(size_t i=0, size=properties.size(); i<size; ++i) ret.sum += properties[i].sum;
		return ret;
	}

	static TestRegionProperties combine_leaf_properties(const std::vector<TestPixelProperties>& properties)
	{
		TestRegionProperties ret;
		for(size_t i=0, size=properties.size(); i<size; ++i) ret.sum += properties[i];
		return ret;
	}
};

//#################### GLOBAL OPERATORS ####################
inline std::ostream& operator<<(std::ostream& os, const TestRegionProperties& rhs)
{
	os << rhs.sum;
	return os;
}

}

#endif
