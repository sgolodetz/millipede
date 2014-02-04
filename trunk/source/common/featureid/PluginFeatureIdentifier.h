/***
 * millipede: MultiFeatureIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PLUGINFEATUREIDENTIFIER
#define H_MILLIPEDE_PLUGINFEATUREIDENTIFIER

#include <common/jobs/CompositeJob.h>
#include "common/featureid/FeatureIdentifier.h"
#include <iostream>

namespace mp {

class PluginFeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	PluginFeatureIdentifier();
	
	//virtual void setParams(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);
	
	void print_string() {
		std::cout << "I'm a plugin" << std::endl;
	}
	
};

}

#endif
