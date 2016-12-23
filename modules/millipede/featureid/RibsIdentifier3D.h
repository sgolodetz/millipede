/***
 * millipede: RibsIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_RIBSIDENTIFIER3D
#define H_MILLIPEDE_RIBSIDENTIFIER3D

#include <common/jobs/SimpleJob.h>
#include "FeatureIdentifier.h"

namespace mp {

class RibsIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	RibsIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
	bool grow_condition(const PFNodeID& adj, const BranchProperties& adjProperties, const BranchProperties& curProperties, const BranchProperties& seedProperties, const BranchProperties& overallProperties) const;
	bool is_seed(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties) const;
	PartitionForestSelection_Ptr postprocess_regions(const PartitionForestSelection_Ptr& preliminaryRegions) const;
};

}

#endif
