/***
 * millipede: MultiFeatureIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PLUGINWRAPPERINTERFACE
#define H_MILLIPEDE_PLUGINWRAPPERINTERFACE
#include "common/featureid/FeatureIdentifier.h"
#include <iostream>

namespace mp {

class PluginWrapperInterface
{
	//#################### CONSTRUCTORS ####################
public:
	typedef VolumeIPF<DICOMImageLeafLayer,DICOMImageBranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef VolumeIPFMultiFeatureSelection<DICOMImageLeafLayer,DICOMImageBranchLayer,AbdominalFeature::Enum> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;
	
	PluginWrapperInterface();
	
	virtual PluginFeatureIdentifier* indentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);
	
	virtual PluginFeatureIdentifier* start_plugin();
};

}

#endif
