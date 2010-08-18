/***
 * millipede: TransformFeatureIdentifier.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TRANSFORMFEATUREIDENTIFIER
#define H_MILLIPEDE_TRANSFORMFEATUREIDENTIFIER

#include "SingleOutputFeatureIdentifier.h"

namespace mp {

class TransformFeatureIdentifier : public SingleOutputFeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	TransformFeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	using SingleOutputFeatureIdentifier::set_mfs_hook;
};

}

#endif
