/***
 * millipede: IPFSelectionGrid.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFSELECTIONGRID
#define H_MILLIPEDE_IPFSELECTIONGRID

#include <common/partitionforests/base/PartitionForestSelection.h>
#include "IPFGrid.h"

namespace mp {

template <typename IPFSelection>
class IPFSelectionGrid : public ForestGrid<IPFSelection>
{
	//#################### CONSTRUCTORS ####################
public:
	template <typename IPFGrid>
	explicit IPFSelectionGrid(const boost::shared_ptr<IPFGrid>& ipfGrid)
	{
		int size = ipfGrid->element_count();
		std::vector<Element_Ptr> selections(size);
		for(int i=0; i<size; ++i)
		{
			selections[i].reset(new IPFSelection(ipfGrid->element(i)));
		}
		initialise(selections, ipfGrid->subvolume_size(), ipfGrid->volume_size());
	}
};

}

#endif
