/***
 * millipede: ITransformFeatureIdentifier.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ITRANSFORMFEATUREIDENTIFIER
#define H_MILLIPEDE_ITRANSFORMFEATUREIDENTIFIER

#include "ISingleOutputFeatureIdentifier.h"

namespace mp {

class ITransformFeatureIdentifier : public ISingleOutputFeatureIdentifier
{
	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook) = 0;
};

}

#endif
