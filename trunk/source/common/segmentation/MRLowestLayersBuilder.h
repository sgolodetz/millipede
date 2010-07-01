/***
 * millipede: MRLowestLayersBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MRLOWESTLAYERSBUILDER
#define H_MILLIPEDE_MRLOWESTLAYERSBUILDER

#include <common/jobs/DataHook.h>
#include <common/jobs/SimpleJob.h>
#include <common/partitionforests/base/PartitionForest.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include "MRSegmentationOptions.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

class MRLowestLayersBuilder : public SimpleJob
{
	//#################### TYPEDEFS ####################
public:
	typedef boost::shared_ptr<DICOMImageBranchLayer> DICOMImageBranchLayer_Ptr;
	typedef boost::shared_ptr<DICOMImageLeafLayer> DICOMImageLeafLayer_Ptr;
	typedef PartitionForest<DICOMImageLeafLayer,DICOMImageBranchLayer> IPF;
	typedef boost::shared_ptr<IPF> IPF_Ptr;
	typedef MRSegmentationOptions SegmentationOptions;

	//#################### PRIVATE VARIABLES ####################
private:
	DICOMImageLeafLayer_Ptr& m_leafLayer;
	DICOMImageBranchLayer_Ptr& m_lowestBranchLayer;
	MRSegmentationOptions m_segmentationOptions;
	DataHook<DICOMVolume_CPtr> m_volumeHook;

	//#################### CONSTRUCTORS ####################
public:
	MRLowestLayersBuilder(const MRSegmentationOptions& segmentationOptions, DICOMImageLeafLayer_Ptr& leafLayer, DICOMImageBranchLayer_Ptr& lowestBranchLayer);

	//#################### PUBLIC METHODS ####################
public:
	void execute();
	int length() const;
	void set_volume_hook(const DataHook<DICOMVolume_CPtr>& volumeHook);
};

}

#endif
