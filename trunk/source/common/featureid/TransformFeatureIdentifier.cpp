/***
 * millipede: TransformFeatureIdentifier.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "TransformFeatureIdentifier.h"

namespace mp {

//#################### CONSTRUCTORS ####################
TransformFeatureIdentifier::TransformFeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	m_dicomVolume(dicomVolume), m_volumeIPF(volumeIPF)
{}

//#################### PUBLIC METHODS ####################
const DataHook<TransformFeatureIdentifier::VolumeIPFMultiFeatureSelection_Ptr>& TransformFeatureIdentifier::get_mfs_hook() const
{
	return m_mfsHook;
}

const TransformFeatureIdentifier::VolumeIPFMultiFeatureSelection_Ptr& TransformFeatureIdentifier::get_multi_feature_selection() const
{
	return m_mfsHook.get();
}

void TransformFeatureIdentifier::set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook)
{
	m_mfsHook = mfsHook;
}

//#################### PROTECTED METHODS ####################
DICOMVolume_CPtr TransformFeatureIdentifier::dicom_volume() const
{
	return m_dicomVolume;
}

TransformFeatureIdentifier::VolumeIPF_CPtr TransformFeatureIdentifier::volume_ipf() const
{
	return m_volumeIPF;
}

}
