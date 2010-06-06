/***
 * millipede: CTIPFBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTIPFBUILDER
#define H_MILLIPEDE_CTIPFBUILDER

#include <common/jobs/SimpleJob.h>
#include <common/partitionforests/base/PartitionForest.h>
#include <common/partitionforests/images/CTImageBranchLayer.h>
#include <common/partitionforests/images/CTImageLeafLayer.h>
#include "CTSegmentationOptions.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
struct CTSegmentationOptions;
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

class CTIPFBuilder : public SimpleJob
{
	//#################### TYPEDEFS ####################
public:
	typedef PartitionForest<CTImageLeafLayer,CTImageBranchLayer> IPF;
	typedef boost::shared_ptr<IPF> IPF_Ptr;
	typedef CTSegmentationOptions SegmentationOptions;

	//#################### PRIVATE VARIABLES ####################
private:
	IPF_Ptr& m_ipf;
	CTSegmentationOptions m_segmentationOptions;
	boost::shared_ptr<DICOMVolume_CPtr> m_volume;

	//#################### CONSTRUCTORS ####################
public:
	CTIPFBuilder(const DICOMVolume_CPtr& volume, const CTSegmentationOptions& segmentationOptions, IPF_Ptr& ipf);
	CTIPFBuilder(const boost::shared_ptr<DICOMVolume_CPtr>& volume, const CTSegmentationOptions& segmentationOptions, IPF_Ptr& ipf);

	//#################### PUBLIC METHODS ####################
public:
	void execute();
	int length() const;
};

}

#endif
