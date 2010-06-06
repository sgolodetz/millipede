/***
 * millipede: IPFGrid.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFGRID
#define H_MILLIPEDE_IPFGRID

#include <common/partitionforests/base/PartitionForest.h>
#include "ForestGrid.h"

namespace mp {

template <typename IPF>
class IPFGrid : public ForestGrid<IPF>
{
	//#################### CONSTRUCTORS ####################
public:
	IPFGrid(const std::vector<boost::shared_ptr<IPF> >& forests, const itk::Size<3>& subvolumeSize, const itk::Size<3>& volumeSize)
	:	ForestGrid<IPF>(forests, subvolumeSize, volumeSize)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int highest_layer() const
	{
		int minHighestLayer = INT_MAX;
		for(int i=0, size=this->element_count(); i<size; ++i)
		{
			int highestLayer = this->element(i)->highest_layer();
			if(highestLayer < minHighestLayer) minHighestLayer = highestLayer;
		}
		return minHighestLayer;
	}
};

}

#endif
