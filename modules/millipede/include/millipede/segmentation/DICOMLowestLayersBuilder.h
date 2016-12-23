/***
 * millipede: DICOMLowestLayersBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMLOWESTLAYERSBUILDER
#define H_MILLIPEDE_DICOMLOWESTLAYERSBUILDER

#include "../jobs/DataHook.h"
#include "../jobs/SimpleJob.h"
#include "../partitionforests/base/PartitionForest.h"
#include "../partitionforests/images/DICOMImageBranchLayer.h"
#include "../partitionforests/images/DICOMImageLeafLayer.h"
#include "DICOMSegmentationOptions.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

class DICOMLowestLayersBuilder : public SimpleJob
{
	//#################### TYPEDEFS ####################
public:
	typedef boost::shared_ptr<DICOMImageBranchLayer> DICOMImageBranchLayer_Ptr;
	typedef boost::shared_ptr<DICOMImageLeafLayer> DICOMImageLeafLayer_Ptr;
	typedef PartitionForest<DICOMImageLeafLayer,DICOMImageBranchLayer> IPF;
	typedef boost::shared_ptr<IPF> IPF_Ptr;
	typedef DICOMSegmentationOptions SegmentationOptions;

	//#################### PRIVATE VARIABLES ####################
private:
	DICOMImageLeafLayer_Ptr& m_leafLayer;
	DICOMImageBranchLayer_Ptr& m_lowestBranchLayer;
	DICOMSegmentationOptions m_segmentationOptions;
	DataHook<DICOMVolume_CPtr> m_volumeHook;

	//#################### CONSTRUCTORS ####################
public:
	DICOMLowestLayersBuilder(const DICOMSegmentationOptions& segmentationOptions, DICOMImageLeafLayer_Ptr& leafLayer, DICOMImageBranchLayer_Ptr& lowestBranchLayer);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;
	void set_volume_hook(const DataHook<DICOMVolume_CPtr>& volumeHook);

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
};

}

#endif
