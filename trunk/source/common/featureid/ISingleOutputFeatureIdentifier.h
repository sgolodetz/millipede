/***
 * millipede: ISingleOutputFeatureIdentifier.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ISINGLEOUTPUTFEATUREIDENTIFIER
#define H_MILLIPEDE_ISINGLEOUTPUTFEATUREIDENTIFIER

#ifdef _MSC_VER
	// Disable the spurious "inherits * via dominance" warnings that VC++ will throw out - the virtual inheritance being used here is entirely intentional.
	#pragma warning(disable:4250)
#endif

#include <common/jobs/DataHook.h>
#include <common/jobs/Job.h>
#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

class ISingleOutputFeatureIdentifier : public virtual Job
{
	//#################### TYPEDEFS ####################
protected:
	typedef DICOMImageBranchLayer BranchLayer;
	typedef AbdominalFeature::Enum Feature;
	typedef DICOMImageLeafLayer LeafLayer;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;
	typedef VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;

	typedef VolumeIPFT::BranchNodeConstIterator BranchNodeConstIterator;
	typedef VolumeIPFT::BranchProperties BranchProperties;
	typedef VolumeIPFT::LeafNodeConstIterator LeafNodeConstIterator;
	typedef VolumeIPFT::LeafProperties LeafProperties;

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& get_mfs_hook() const = 0;
	virtual const VolumeIPFMultiFeatureSelection_Ptr& get_multi_feature_selection() const = 0;
};

}

#endif
