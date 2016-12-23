/***
 * millipede: SpleenIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SPLEENIDENTIFIER3D
#define H_MILLIPEDE_SPLEENIDENTIFIER3D

#include <common/jobs/SimpleJob.h>
#include "FeatureIdentifier.h"

namespace mp {

class SpleenIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	SpleenIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
	bool is_candidate(const PFNodeID& node, const BranchProperties& properties) const;
};

}

#endif
