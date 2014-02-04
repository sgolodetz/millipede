/***
 * millipede: JMultiFeatureIdentifier3D.cpp
 * Jess Pumphrey, 2012
 ***/

#include "JMultiFeatureIdentifier3D.h"

#include "JAortaIdentifier3D.h"
#include "JKidneysIdentifier3D.h"
#include "JLiverIdentifier3D.h"
#include "JRibsIdentifier3D.h"
#include "JSpinalCordIdentifier3D.h"
#include "JSpineIdentifier3D.h"
#include "JSpleenIdentifier3D.h"

#include <iostream>

namespace mp {

//#################### CONSTRUCTORS ####################
JMultiFeatureIdentifier3D::JMultiFeatureIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, const FIOptions map2)
:	FeatureIdentifier(dicomVolume, volumeIPF)
{
	FIOptions map = map2;
	
	std::cout << map["KMaxVox"] << "value from map" << std::endl;
	
	int doSpine = map["DoSpine"];
	int doSpinal = map["DoSpinal"];
	int doRibs = map["DoRibs"];
	int doAorta = map["DoAorta"];
	int doLiver = map["DoLiver"];
	int doKidneys = map["DoKidneys"];
	int doSpleen = map["DoSpleen"];
	
	if (doSpine == 1) {
		// Identify the spine, spinal cord and ribs (these can be used as references).
		JSpineIdentifier3D *spineIdentifier = new JSpineIdentifier3D(dicomVolume, volumeIPF);
		spineIdentifier->set_mfs_hook(get_mfs_hook());
		add_subjob(spineIdentifier);
	}
	
	if (doSpinal == 1) {
		JSpinalCordIdentifier3D *spinalCordIdentifier = new JSpinalCordIdentifier3D(dicomVolume, volumeIPF);
		spinalCordIdentifier->set_mfs_hook(get_mfs_hook());
		add_subjob(spinalCordIdentifier);
	}
	
	if (doRibs == 1) {

		JRibsIdentifier3D *ribsIdentifier = new JRibsIdentifier3D(dicomVolume, volumeIPF, map);
		ribsIdentifier->set_mfs_hook(get_mfs_hook());
		add_subjob(ribsIdentifier);
	}
	
	if (doAorta == 1) {

		// Identify major blood vessels like the aorta.
		JAortaIdentifier3D *aortaIdentifier = new JAortaIdentifier3D(dicomVolume, volumeIPF, map);
		aortaIdentifier->set_mfs_hook(get_mfs_hook());
		add_subjob(aortaIdentifier);
	}
	
	if (doLiver == 1) {

		// Identify soft tissue organs.
		JLiverIdentifier3D *liverIdentifier = new JLiverIdentifier3D(dicomVolume, volumeIPF, map);
		liverIdentifier->set_mfs_hook(get_mfs_hook());
		add_subjob(liverIdentifier);
	}
	
	if (doKidneys == 1) {

		JKidneysIdentifier3D *kidneysIdentifier = new JKidneysIdentifier3D(dicomVolume, volumeIPF, map);
		kidneysIdentifier->set_mfs_hook(get_mfs_hook());
		add_subjob(kidneysIdentifier);
	}
	
	if (doSpleen == 1) {

		JSpleenIdentifier3D *spleenIdentifier = new JSpleenIdentifier3D(dicomVolume, volumeIPF);
		spleenIdentifier->set_mfs_hook(get_mfs_hook());
		add_subjob(spleenIdentifier);
	}
}

}
