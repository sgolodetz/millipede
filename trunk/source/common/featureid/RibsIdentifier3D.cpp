/***
 * millipede: RibsIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "RibsIdentifier3D.h"

#include <boost/bind.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
RibsIdentifier3D::RibsIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	FeatureIdentifier(dicomVolume, volumeIPF)
{}

//#################### PUBLIC METHODS ####################
int RibsIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void RibsIdentifier3D::execute_impl()
{
	set_status("Identifying ribs...");

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	// Step 1: Calculate the combined properties of all the nodes marked as part of the spine.
	BranchProperties spineProperties = multiFeatureSelection->properties_of(AbdominalFeature::VERTEBRA);

	// Step 2: Filter for rib nodes.
	std::list<PFNodeID> nodes = filter_branch_nodes(boost::bind(&RibsIdentifier3D::is_rib, this, _1, _2, spineProperties));

#if 0
	// Step 3: Mark the resulting nodes as rib (and unmark them as spine if necessary).
	for(std::list<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		multiFeatureSelection->identify_node(*it, AbdominalFeature::RIB);
		multiFeatureSelection->unidentify_node(*it, AbdominalFeature::VERTEBRA);
	}
#endif
}

bool RibsIdentifier3D::is_rib(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties) const
{
	int minVoxelsPerSlice = 90;
	int maxVoxelsPerSlice = 500;
	int sliceCount = properties.z_max() + 1 - properties.z_min();
	double aspectRatio = properties.aspect_ratio();

	return	properties.mean_grey_value() >= 190 &&							// it should have a high grey value
			0.25 <= aspectRatio && aspectRatio <= 4 &&						// it should have a reasonable aspect ratio
			properties.voxel_count() >= minVoxelsPerSlice * sliceCount &&	// it should have a reasonable size
			properties.voxel_count() <= maxVoxelsPerSlice * sliceCount &&
			(properties.x_min() <= spineProperties.x_min() ||				// and it should not be strictly contained within the spine in the x axis
			 properties.x_max() >= spineProperties.x_max());
}

}
