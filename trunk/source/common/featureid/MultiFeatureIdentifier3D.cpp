/***
 * millipede: MultiFeatureIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MultiFeatureIdentifier3D.h"

#include "SpinalCordIdentifier3D.h"
#include "SpineIdentifier3D.h"

namespace mp {

//#################### CONSTRUCTORS ####################
MultiFeatureIdentifier3D::MultiFeatureIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
{
	SpineIdentifier3D *spineIdentifier = new SpineIdentifier3D(dicomVolume, volumeIPF);
	add_subjob(spineIdentifier);

	SpinalCordIdentifier3D *spinalCordIdentifier = new SpinalCordIdentifier3D(dicomVolume, volumeIPF);
	spinalCordIdentifier->set_mfs_hook(spineIdentifier->get_mfs_hook());
	add_subjob(spinalCordIdentifier);

	m_mfsHook = spinalCordIdentifier->get_mfs_hook();
}

//#################### PUBLIC METHODS ####################
const MultiFeatureIdentifier3D::VolumeIPFMultiFeatureSelection_Ptr& MultiFeatureIdentifier3D::get_output() const
{
	return m_mfsHook.get();
}

}
