/***
 * millipede: FeatureIdentifier.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "FeatureIdentifier.h"

namespace mp {

//#################### CONSTRUCTORS ####################
FeatureIdentifier::FeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	m_dicomVolume(dicomVolume), m_volumeIPF(volumeIPF)
{}

//#################### PROTECTED METHODS ####################
DICOMVolume_CPtr FeatureIdentifier::dicom_volume() const
{
	return m_dicomVolume;
}

FeatureIdentifier::VolumeIPF_CPtr FeatureIdentifier::volume_ipf() const
{
	return m_volumeIPF;
}

}
