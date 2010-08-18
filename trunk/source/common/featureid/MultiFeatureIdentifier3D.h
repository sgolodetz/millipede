/***
 * millipede: MultiFeatureIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MULTIFEATUREIDENTIFIER3D
#define H_MILLIPEDE_MULTIFEATUREIDENTIFIER3D

#include <common/jobs/CompositeJob.h>
#include "SingleOutputFeatureIdentifier.h"

namespace mp {

class MultiFeatureIdentifier3D : public CompositeJob, public SingleOutputFeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	MultiFeatureIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);
};

}

#endif
