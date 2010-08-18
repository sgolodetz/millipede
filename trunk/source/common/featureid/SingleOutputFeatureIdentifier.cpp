/***
 * millipede: SingleOutputFeatureIdentifier.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SingleOutputFeatureIdentifier.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SingleOutputFeatureIdentifier::SingleOutputFeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	FeatureIdentifier(dicomVolume, volumeIPF)
{
	m_mfsHook.set(VolumeIPFMultiFeatureSelection_Ptr(new VolumeIPFMultiFeatureSelectionT(volumeIPF)));
}

//#################### PUBLIC METHODS ####################
const DataHook<SingleOutputFeatureIdentifier::VolumeIPFMultiFeatureSelection_Ptr>& SingleOutputFeatureIdentifier::get_mfs_hook() const
{
	return m_mfsHook;
}

const SingleOutputFeatureIdentifier::VolumeIPFMultiFeatureSelection_Ptr& SingleOutputFeatureIdentifier::get_multi_feature_selection() const
{
	return m_mfsHook.get();
}

//#################### PROTECTED METHODS ####################
void SingleOutputFeatureIdentifier::set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook)
{
	m_mfsHook = mfsHook;
}

}
