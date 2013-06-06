/***
 * millipede: MultiFeatureIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PLUGIN_WRAPPER
#define H_MILLIPEDE_PLUGIN_WRAPPER

#include "common/featureid/PluginFeatureIdentifier.h"
#include <iostream>

namespace mp {

class PluginWrapper
{
	//#################### CONSTRUCTORS ####################
public:
	PluginWrapper() {};
	
	PluginFeatureIdentifier * indentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF) {
		return new PluginMultiFeatureIdentifier3D(dicomVolume, volumeIPF);
	}
	
	/*PluginFeatureIdentifier * start_plugin() {
		return new PluginWrapper();
	}*/
	
	void print_string() {
		std::cout << "I'm a specific plugin" << std::endl;
	}
	
};

extern "C" {
	PluginWrapper * start_plugin() {
		return new PluginWrapper();
	}
}

}

#endif
