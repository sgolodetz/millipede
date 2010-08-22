/***
 * millipede: MultiFeatureIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MultiFeatureIdentifier3D.h"

#include "AortaIdentifier3D.h"
#include "KidneysIdentifier3D.h"
#include "LiverIdentifier3D.h"
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

	LiverIdentifier3D *liverIdentifier = new LiverIdentifier3D(dicomVolume, volumeIPF);
	liverIdentifier->set_mfs_hook(get_mfs_hook());
	add_subjob(liverIdentifier);

	KidneysIdentifier3D *kidneysIdentifier = new KidneysIdentifier3D(dicomVolume, volumeIPF);
	kidneysIdentifier->set_mfs_hook(get_mfs_hook());
	add_subjob(kidneysIdentifier);

	AortaIdentifier3D *aortaIdentifier = new AortaIdentifier3D(dicomVolume, volumeIPF);
	aortaIdentifier->set_mfs_hook(get_mfs_hook());
	add_subjob(aortaIdentifier);
}

}
