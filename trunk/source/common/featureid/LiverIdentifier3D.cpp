/***
 * millipede: LiverIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LiverIdentifier3D.h"

#include <climits>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### CONSTRUCTORS ####################
LiverIdentifier3D::LiverIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	StratifiedRegionGrowingFeatureIdentifier(dicomVolume, volumeIPF)
{}

//#################### PUBLIC METHODS ####################
int LiverIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void LiverIdentifier3D::execute_impl()
{
	set_status("Identifying liver...");

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	// Step 1: Filter for the liver candidates.
	std::list<PFNodeID> candidates = filter_branch_nodes(boost::bind(&LiverIdentifier3D::is_liver_candidate, this, _1, _2));

	// Step 2: Pick the best candidate (namely, the one which stretches furthest to the left of the image).
	PFNodeID bestCandidate;
	int bestXMin = INT_MAX;
	for(std::list<PFNodeID>::const_iterator it=candidates.begin(), iend=candidates.end(); it!=iend; ++it)
	{
		const BranchProperties& properties = volume_ipf()->branch_properties(*it);
		if(properties.x_min() < bestXMin)
		{
			bestCandidate = *it;
			bestXMin = properties.x_min();
		}
	}

	// If we can't find a candidate liver, exit.
	if(bestCandidate == PFNodeID::invalid()) return;

	// Step 3: (TEMPORARY) Identify it as liver.
	multiFeatureSelection->identify_node(bestCandidate, AbdominalFeature::LIVER);
}

bool LiverIdentifier3D::is_liver_candidate(const PFNodeID& node, const BranchProperties& properties) const
{
	itk::Index<3> volumeSize = ITKImageUtil::make_index_from_size(dicom_volume()->size());
	int minVoxelsPerSlice = 800;
	int sliceCount = properties.z_max() + 1 - properties.z_min();

	return	node.layer() == 1 &&											// it should be in the lowest branch layer (higher nodes generally stretch too far)
			properties.voxel_count() >= minVoxelsPerSlice * sliceCount &&	// it should be reasonably-sized
			properties.mean_grey_value() >= 150 &&							// it should be reasonably bright
			properties.x_min() < volumeSize[0]/2;							// and at least part of it should be on the left-hand side of the image
}

}
