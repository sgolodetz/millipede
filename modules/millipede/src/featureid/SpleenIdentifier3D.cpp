/***
 * millipede: SpleenIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "featureid/SpleenIdentifier3D.h"

#include <climits>

#include <boost/bind.hpp>

#include "dicom/volumes/DICOMVolume.h"
#include "util/ITKImageUtil.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SpleenIdentifier3D::SpleenIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	FeatureIdentifier(dicomVolume, volumeIPF)
{}

//#################### PUBLIC METHODS ####################
int SpleenIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void SpleenIdentifier3D::execute_impl()
{
	set_status("Identifying spleen...");

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	// Step 1: Filter for spleen candidates.
	std::list<PFNodeID> candidates = filter_branch_nodes(boost::bind(&SpleenIdentifier3D::is_candidate, this, _1, _2));

	// Step 2: Pick the best candidate (namely, the one which stretches furthest to the right of the image).
	PFNodeID bestCandidate;
	int bestXMax = INT_MIN;
	for(std::list<PFNodeID>::const_iterator it=candidates.begin(), iend=candidates.end(); it!=iend; ++it)
	{
		const BranchProperties& properties = volume_ipf()->branch_properties(*it);
		if(properties.x_max() > bestXMax)
		{
			bestCandidate = *it;
			bestXMax = properties.x_max();
		}
	}

	// If we can't find a candidate spleen, exit.
	if(bestCandidate == PFNodeID::invalid()) return;

	// Step 3: (TEMPORARY) Mark the best candidate as spleen (and unmark it as liver if necessary).
	multiFeatureSelection->identify_node(bestCandidate, AbdominalFeature::SPLEEN);
	multiFeatureSelection->unidentify_node(bestCandidate, AbdominalFeature::LIVER);
}

bool SpleenIdentifier3D::is_candidate(const PFNodeID& node, const BranchProperties& properties) const
{
	itk::Index<3> volumeSize = ITKImageUtil::make_index_from_size(dicom_volume()->size());
	int sliceCount = properties.z_max() + 1 - properties.z_min();
	int minVoxelsPerSlice = 1000;
	int maxVoxelsPerSlice = 7000;

	return	node.layer() == 1 &&															// it must be in the lowest branch layer
			140 <= properties.mean_grey_value() && properties.mean_grey_value() <= 170 &&	// it must have a reasonable grey value
			properties.x_max() >= volumeSize[0]*0.8 &&										// it must stretch sufficiently far to the right
			properties.voxel_count() >= minVoxelsPerSlice * sliceCount &&					// it must be of a reasonable size
			properties.voxel_count() <= maxVoxelsPerSlice * sliceCount;
}

}
