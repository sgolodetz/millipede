/***
 * millipede: SpineIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SPINEIDENTIFIER3D
#define H_MILLIPEDE_SPINEIDENTIFIER3D

#include <common/jobs/SimpleJob.h>
#include "SingleOutputFeatureIdentifier.h"

namespace mp {

class SpineIdentifier3D : public SimpleJob, public SingleOutputFeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	SpineIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
	bool is_spine(const BranchProperties& properties) const;
};

}

#endif
