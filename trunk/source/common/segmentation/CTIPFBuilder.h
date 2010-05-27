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
	typedef PartitionForest<CTImageLeafLayer,CTImageBranchLayer> CTIPF;
	typedef boost::shared_ptr<CTIPF> CTIPF_Ptr;
	typedef CTSegmentationOptions SegmentationOptions;

	//#################### PRIVATE VARIABLES ####################
private:
	CTIPF_Ptr& m_ipf;
	CTSegmentationOptions m_segmentationOptions;
	DICOMVolume_CPtr m_volume;

	//#################### CONSTRUCTORS ####################
public:
	CTIPFBuilder(const DICOMVolume_CPtr& volume, const CTSegmentationOptions& segmentationOptions, CTIPF_Ptr& ipf);

	//#################### PUBLIC METHODS ####################
public:
	void execute();
	int length() const;
};

}

#endif
