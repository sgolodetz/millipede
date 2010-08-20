/***
 * millipede: LiverIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LiverIdentifier3D.h"

#include <climits>

#include <boost/bind.hpp>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### CONSTRUCTORS ####################
LiverIdentifier3D::LiverIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	FeatureIdentifier(dicomVolume, volumeIPF)
{}

//#################### PUBLIC METHODS ####################
int LiverIdentifier3D::length() const
{
	return 5;
}

//#################### PRIVATE METHODS ####################
void LiverIdentifier3D::execute_impl()
{
	set_status("Identifying liver...");

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	// Step 1: Filter for the liver candidates.
	std::list<PFNodeID> candidates = filter_branch_nodes(boost::bind(&LiverIdentifier3D::is_liver_candidate, this, _1, _2));
	increment_progress();

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

	increment_progress();

	// Step 3: Grow a region from the candidate liver.
	std::list<PFNodeID> bestCandidateList;
	bestCandidateList.push_back(bestCandidate);
	PartitionForestSelection_Ptr region = grow_regions(bestCandidateList, boost::bind(&LiverIdentifier3D::grow_condition, this, _1, _2, _3, _4, _5));
	increment_progress();

	// Step 4: Use conditional morphological closing to try and fill any holes.
	std::set<PFNodeID> nodes(region->view_at_layer_cbegin(1), region->view_at_layer_cend(1));
	morphologically_close_nodes(nodes, boost::bind(&LiverIdentifier3D::morphological_condition, this, _1));
	PartitionForestSelection_Ptr filledRegion(new PartitionForestSelectionT(volume_ipf()));
	for(std::set<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		filledRegion->select_node(*it);
	}
	increment_progress();

	// Step 5: Mark the result as liver.
	multiFeatureSelection->identify_selection(filledRegion, AbdominalFeature::LIVER);
}

bool LiverIdentifier3D::grow_condition(const PFNodeID& adj, const BranchProperties& adjProperties, const BranchProperties& curProperties,
									   const BranchProperties& seedProperties, const BranchProperties& overallProperties) const
{
	int sliceCount = adjProperties.z_max() + 1 - adjProperties.z_min();

	// FIXME: The size condition is a hack to try and prevent excessive flooding (a better approach needs to be found).
	return	adjProperties.x_min() >= seedProperties.x_min() &&									// it should be no further left than the original seed
			fabs(adjProperties.mean_grey_value() - seedProperties.mean_grey_value()) < 15 &&	// it should be roughly the same grey value as the seed
			fabs(adjProperties.mean_grey_value() - curProperties.mean_grey_value()) < 10 &&		// it should be roughly the same grey value as its generating region
			adjProperties.voxel_count() <= 150 * sliceCount;									// it shouldn't be too large
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

bool LiverIdentifier3D::morphological_condition(const BranchProperties& properties) const
{
	return properties.mean_grey_value() >= 140;
}

}
