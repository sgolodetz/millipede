/***
 * millipede: CTLowestLayersBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTLOWESTLAYERSBUILDER
#define H_MILLIPEDE_CTLOWESTLAYERSBUILDER

#include <common/jobs/SimpleJob.h>
#include <common/partitionforests/base/PartitionForest.h>
#include <common/partitionforests/images/CTMRImageBranchLayer.h>
#include <common/partitionforests/images/CTMRImageLeafLayer.h>
#include "CTSegmentationOptions.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
struct CTSegmentationOptions;
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

class CTLowestLayersBuilder : public SimpleJob
{
	//#################### TYPEDEFS ####################
public:
	typedef boost::shared_ptr<CTMRImageBranchLayer> CTMRImageBranchLayer_Ptr;
	typedef boost::shared_ptr<CTMRImageLeafLayer> CTMRImageLeafLayer_Ptr;
	typedef PartitionForest<CTMRImageLeafLayer,CTMRImageBranchLayer> IPF;
	typedef boost::shared_ptr<IPF> IPF_Ptr;
	typedef CTSegmentationOptions SegmentationOptions;

	//#################### PRIVATE VARIABLES ####################
private:
	CTMRImageLeafLayer_Ptr& m_leafLayer;
	CTMRImageBranchLayer_Ptr& m_lowestBranchLayer;
	CTSegmentationOptions m_segmentationOptions;
	boost::shared_ptr<DICOMVolume_CPtr> m_volume;

	//#################### CONSTRUCTORS ####################
public:
	CTLowestLayersBuilder(const DICOMVolume_CPtr& volume, const CTSegmentationOptions& segmentationOptions, CTMRImageLeafLayer_Ptr& leafLayer, CTMRImageBranchLayer_Ptr& lowestBranchLayer);
	CTLowestLayersBuilder(const boost::shared_ptr<DICOMVolume_CPtr>& volume, const CTSegmentationOptions& segmentationOptions, CTMRImageLeafLayer_Ptr& leafLayer, CTMRImageBranchLayer_Ptr& lowestBranchLayer);

	//#################### PUBLIC METHODS ####################
public:
	void execute();
	int length() const;
};

}

#endif
