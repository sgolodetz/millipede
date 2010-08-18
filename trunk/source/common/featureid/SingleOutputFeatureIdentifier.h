/***
 * millipede: SingleOutputFeatureIdentifier.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SINGLEOUTPUTFEATUREIDENTIFIER
#define H_MILLIPEDE_SINGLEOUTPUTFEATUREIDENTIFIER

#include <common/jobs/DataHook.h>
#include "FeatureIdentifier.h"

namespace mp {

class SingleOutputFeatureIdentifier : public FeatureIdentifier
{
	//#################### PRIVATE VARIABLES ####################
private:
	DataHook<VolumeIPFMultiFeatureSelection_Ptr> m_mfsHook;

	//#################### CONSTRUCTORS ####################
public:
	SingleOutputFeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& get_mfs_hook() const;
	const VolumeIPFMultiFeatureSelection_Ptr& get_multi_feature_selection() const;

	//#################### PROTECTED METHODS ####################
protected:
	void set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook);
};

}

#endif
