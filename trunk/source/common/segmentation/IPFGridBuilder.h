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
private:
	typedef typename IPFBuilder::IPF IPF;
	typedef typename IPFBuilder::IPF_Ptr IPF_Ptr;
	typedef typename IPFGrid<IPF> IPFG;
	typedef boost::shared_ptr<IPFG> IPFG_Ptr;
	typedef typename IPFBuilder::SegmentationOptions SegmentationOptions;

	//#################### NESTED CLASSES ####################
private:
	struct ExtractSubvolumeJob : SimpleJob
	{
		// TODO
	};

	//#################### PRIVATE VARIABLES ####################
private:
	// TODO

	//#################### CONSTRUCTORS ####################
public:
	IPFGridBuilder(const DICOMVolume_CPtr& volume, const SegmentationOptions& segmentationOptions, IPFG_Ptr& ipfGrid)
	{
		// NYI
		throw 23;
	}
};

}

#endif
