/***
 * millipede: MultiFeatureIdentifier3D.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MULTIFEATUREIDENTIFIER3D
#define H_MILLIPEDE_MULTIFEATUREIDENTIFIER3D

#include <common/jobs/CompositeJob.h>
#include <common/jobs/DataHook.h>
#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

class MultiFeatureIdentifier3D : public CompositeJob
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

	//#################### PRIVATE VARIABLES ####################
private:
	DataHook<VolumeIPFMultiFeatureSelection_Ptr> m_outputHook;

	//#################### CONSTRUCTORS ####################
public:
	MultiFeatureIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	const VolumeIPFMultiFeatureSelection_Ptr& get_output() const;
	const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& get_output_hook() const;
};

}

#endif
