/***
 * millipede: KidneysIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_KIDNEYSIDENTIFIER3D
#define H_MILLIPEDE_KIDNEYSIDENTIFIER3D

#include "../jobs/SimpleJob.h"
#include "FeatureIdentifier.h"

namespace mp {

class KidneysIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	KidneysIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
	bool is_kidney(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties) const;
};

}

#endif
