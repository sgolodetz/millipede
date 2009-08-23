/***
 * millipede: RegionProperties.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_REGIONPROPERTIES
#define H_MILLIPEDE_REGIONPROPERTIES

#include <vector>

#include "VoxelProperties.h"

namespace mp {

struct RegionProperties
{
	int area;
	double meanGreyValue;

	RegionProperties() : area(0), meanGreyValue(0) {}
	explicit RegionProperties(int area_, double meanGreyValue_) : area(area_), meanGreyValue(meanGreyValue_) {}

	static RegionProperties combine(const std::vector<RegionProperties>& childProperties)
	{
		// Sample implementation

		if(childProperties.empty()) /* NYI */ throw 23;

		int areaSum = 0;
		double greyValueSum = 0;
		for(size_t i=0, size=childProperties.size(); i<size; ++i)
		{
			areaSum += childProperties[i].area;
			greyValueSum += childProperties[i].area * childProperties[i].meanGreyValue;
		}
		return RegionProperties(areaSum, greyValueSum / areaSum);
	}

	static RegionProperties combine(const std::vector<VoxelProperties>& childProperties)
	{
		// Sample implementation

		if(childProperties.empty()) /* NYI */ throw 23;

		double sum = 0;
		for(size_t i=0, size=childProperties.size(); i<size; ++i)
		{
			sum += childProperties[i].greyValue;
		}
		return RegionProperties((int)childProperties.size(), sum / childProperties.size());
	}
};

}

#endif
