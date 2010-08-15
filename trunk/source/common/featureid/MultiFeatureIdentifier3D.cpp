/***
 * millipede: MultiFeatureIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MultiFeatureIdentifier3D.h"

#include "SpineIdentifier3D.h"

namespace mp {

//#################### CONSTRUCTORS ####################
MultiFeatureIdentifier3D::MultiFeatureIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
{
	SpineIdentifier3D *spineIdentifier = new SpineIdentifier3D(dicomVolume, volumeIPF);
	add_subjob(spineIdentifier);

	m_outputHook = spineIdentifier->get_output_hook();
}

//#################### PUBLIC METHODS ####################
const MultiFeatureIdentifier3D::VolumeIPFMultiFeatureSelection_Ptr& MultiFeatureIdentifier3D::get_output() const
{
	return m_outputHook.get();
}

const DataHook<MultiFeatureIdentifier3D::VolumeIPFMultiFeatureSelection_Ptr>& MultiFeatureIdentifier3D::get_output_hook() const
{
	return m_outputHook;
}

}
