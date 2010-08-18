/***
 * millipede: MultiFeatureIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MultiFeatureIdentifier3D.h"

#include "RibsIdentifier3D.h"
#include "SpinalCordIdentifier3D.h"
#include "SpineIdentifier3D.h"

namespace mp {

//#################### CONSTRUCTORS ####################
MultiFeatureIdentifier3D::MultiFeatureIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	FeatureIdentifier(dicomVolume, volumeIPF)
{
	SpineIdentifier3D *spineIdentifier = new SpineIdentifier3D(dicomVolume, volumeIPF);
	spineIdentifier->set_mfs_hook(get_mfs_hook());
	add_subjob(spineIdentifier);

	SpinalCordIdentifier3D *spinalCordIdentifier = new SpinalCordIdentifier3D(dicomVolume, volumeIPF);
	spinalCordIdentifier->set_mfs_hook(get_mfs_hook());
	add_subjob(spinalCordIdentifier);

	RibsIdentifier3D *ribsIdentifier = new RibsIdentifier3D(dicomVolume, volumeIPF);
	ribsIdentifier->set_mfs_hook(get_mfs_hook());
	add_subjob(ribsIdentifier);
}

}
