/***
 * millipede: SpineIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SPINEIDENTIFIER3D
#define H_MILLIPEDE_SPINEIDENTIFIER3D

#include <common/jobs/DataHook.h>
#include <common/jobs/SimpleJob.h>
#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

class SpineIdentifier3D : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef DICOMImageBranchLayer BranchLayer;
	typedef AbdominalFeature::Enum Feature;
	typedef DICOMImageLeafLayer LeafLayer;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;

	typedef VolumeIPFT::BranchNodeConstIterator BranchNodeConstIterator;
	typedef VolumeIPFT::BranchProperties BranchProperties;

	//#################### PRIVATE VARIABLES ####################
private:
	DICOMVolume_CPtr m_dicomVolume;
	DataHook<VolumeIPFMultiFeatureSelection_Ptr> m_outputHook;
	VolumeIPF_Ptr m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	SpineIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	const VolumeIPFMultiFeatureSelection_Ptr& get_output() const;
	const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& get_output_hook() const;
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
	PFNodeID find_seed() const;
};

}

#endif
