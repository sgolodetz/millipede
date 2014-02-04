/***
 * millipede: MultiFeatureIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PLUGINMANAGER
#define H_MILLIPEDE_PLUGINMANAGER

#include <common/jobs/Job.h>
#include <iostream>
#include <common/featureid/PluginFeatureIdentifier.h>
#include <common/featureid/PluginWrapperInterface.h>

#include <dlfcn.h>

namespace mp {

class PluginManager
{
	//#################### CONSTRUCTORS ####################
public:
	typedef VolumeIPF<DICOMImageLeafLayer,DICOMImageBranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef VolumeIPFMultiFeatureSelection<DICOMImageLeafLayer,DICOMImageBranchLayer,AbdominalFeature::Enum> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;
	
	PluginManager();
	
	bool load_plugin(char* name) {
		
		void *hndl = dlopen(name, RTLD_NOW);
		if(hndl == NULL){
			std::cerr << dlerror() << std::endl;
			exit(-1);
		}
		
		//funcp  = (int (*)(int)) dlsym(hndl, "start_plugin"); 
		
		*m_maker = dlsym(hndl, "start_plugin");
		
		return true;
	}
	
	boost::shared_ptr<Job> feature_id_job(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF) {
		
		typedef PluginFeatureIdentifier* Func();
		typedef Func* Func2;
		
		m_plugin = (static_cast<Func2>(m_maker))();
		  
		*m_identifier = m_plugin->identifier(m_dicomVolume, m_volumeIPF);
		
		return boost::shared_ptr<Job>(m_identifier);
		
	}
	
	VolumeIPFMultiFeatureSelection_Ptr& multifeature_selection() {
		
		return m_identifier->get_multi_feature_selection();
	}
	
	void print_string() {
		m_identifier->print_string();
	}
	
private:
	
	//int (*funcp)(int); /* Ptr to function with 1 int arg. */
	PluginFeatureIdentifier * m_identifier;
	PluginWrapperInterface * m_plugin;
	void ** m_maker;
	
	DICOMVolume_CPtr m_dicomVolume;
	VolumeIPF_Ptr m_volumeIPF;
};


	
}

#endif


