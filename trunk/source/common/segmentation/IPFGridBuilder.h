/***
 * millipede: IPFGridBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFGRIDBUILDER
#define H_MILLIPEDE_IPFGRIDBUILDER

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/jobs/CompositeJob.h>
#include <common/partitionforests/images/IPFGrid.h>

namespace mp {

template <typename IPFBuilder>
class IPFGridBuilder : public CompositeJob
{
	//#################### TYPEDEFS ####################
public:
	typedef typename IPFBuilder::SegmentationOptions SegmentationOptions;

	//#################### PRIVATE VARIABLES ####################
private:
	// TODO

	//#################### CONSTRUCTORS ####################
public:
	IPFGridBuilder(const DICOMVolume_CPtr& volume, const SegmentationOptions& segmentationOptions)
	{
		// NYI
		throw 23;
	}
};

}

#endif
