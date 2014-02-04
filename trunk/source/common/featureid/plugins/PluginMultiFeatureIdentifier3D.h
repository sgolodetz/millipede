/***
 * millipede: MultiFeatureIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PLUGIN_MULTIFEATUREIDENTIFIER3D
#define H_MILLIPEDE_PLUGIN_MULTIFEATUREIDENTIFIER3D

#include "common/featureid/PluginFeatureIdentifier.h"
#include <iostream>

namespace mp {

class PluginMultiFeatureIdentifier3D : public PluginFeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	PluginMultiFeatureIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);
	
	PluginFeatureIdentifier * start_plugin(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF) {
		return new PluginMultiFeatureIdentifier3D(dicomVolume, volumeIPF);
	}
	
	void print_string() {
		std::cout << "I'm a specific plugin" << std::endl;
	}
	
};

extern "C" {
PluginMultiFeatureIdentifier3D  * start_plugin(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF) {
		return new PluginMultiFeatureIdentifier3D(dicomVolume, volumeIPF);
	}
}

}

#endif
