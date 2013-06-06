/***
 * millipede: SettingsCapturer.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_SETTINGSCAPTURER
#define H_MILLIPEDE_SETTINGSCAPTURER

#include <common/jobs/CompositeJob.h>
#include <common/jobs/Job.h>
#include <common/jobs/SimpleJob.h>
#include <string>
#include <common/featureid/FIOptions.h>
#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/VolumeIPF.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>
#include <common/partitionforests/base/PartitionForestSelection.h>


namespace mp {

typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;
typedef PartitionForestSelection<DICOMImageLeafLayer,DICOMImageBranchLayer> PartitionForestSelectionT;
typedef boost::shared_ptr<const PartitionForestSelectionT> PartitionForestSelection_CPtr;
typedef VolumeIPF<DICOMImageLeafLayer,DICOMImageBranchLayer> VolumeIPFT;
typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
typedef VolumeIPFMultiFeatureSelection<DICOMImageLeafLayer,DICOMImageBranchLayer,AbdominalFeature::Enum> VolumeIPFMultiFeatureSelectionT;
typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;
typedef VolumeIPFT::BranchProperties BranchProperties;

/**
 * Tries to extract settings for Jess' testing version of the feature identifier, given a multifeature selection
 * identifying the Aorta, Liver and Kidneys
 * 
 * Finds settings for maximum and minimum Houndsfield values, as well as seed and adjacent tolerances for region-grown features.
 * 
 */
class SettingsCapturer : public SimpleJob
{
	//#################### CONSTRUCTORS ####################
public:
	SettingsCapturer(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, FIOptions * map, VolumeIPFMultiFeatureSelection_Ptr mfs);
	
	int length() const {return 1;}
	void execute_impl();
	
	///Parameter for indicating whether the organ's seed is the leftmost, rightmost, top or bottom node.
	enum Seed{ TOP, BOTTOM, LEFT, RIGHT };
	
private:
	
	
	///gets min and max Houndsfield values for non-grown features
	void get_feature_settings(AbdominalFeature::Enum feature, string organPrefix, int layer);
	
	///gets min and max Houndsfield values, and tolerances for grown features
	void get_grow_settings(AbdominalFeature::Enum feature, string organPrefix, int layer, Seed seed);
	
	FIOptions * m_map;
	DICOMVolume_CPtr m_dicomVolume;
	VolumeIPF_Ptr m_volumeIPF;
	VolumeIPFMultiFeatureSelection_Ptr m_mfs;
};

}

#endif
