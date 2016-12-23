/***
 * millipede: AortaIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_AORTAIDENTIFIER3D
#define H_MILLIPEDE_AORTAIDENTIFIER3D

#include <millipede/jobs/SimpleJob.h>
#include "FeatureIdentifier.h"

namespace mp {

class AortaIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	AortaIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
	bool is_seed(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, const BranchProperties& spinalCordProperties) const;
	bool morphological_condition(const BranchProperties& properties) const;
};

}

#endif
