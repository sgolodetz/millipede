/***
 * millipede: FeatureIdentifier.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "FeatureIdentifier.h"

namespace mp {

//#################### CONSTRUCTORS ####################
FeatureIdentifier::FeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	m_dicomVolume(dicomVolume), m_volumeIPF(volumeIPF)
{
	m_mfsHook.set(VolumeIPFMultiFeatureSelection_Ptr(new VolumeIPFMultiFeatureSelectionT(volumeIPF)));
}

//#################### PUBLIC METHODS ####################
const DataHook<FeatureIdentifier::VolumeIPFMultiFeatureSelection_Ptr>& FeatureIdentifier::get_mfs_hook() const
{
	return m_mfsHook;
}

const FeatureIdentifier::VolumeIPFMultiFeatureSelection_Ptr& FeatureIdentifier::get_multi_feature_selection() const
{
	return m_mfsHook.get();
}

void FeatureIdentifier::set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook)
{
	m_mfsHook = mfsHook;
}

//#################### PROTECTED METHODS ####################
DICOMVolume_CPtr FeatureIdentifier::dicom_volume() const
{
	return m_dicomVolume;
}

FeatureIdentifier::VolumeIPF_Ptr FeatureIdentifier::volume_ipf() const
{
	return m_volumeIPF;
}

}
