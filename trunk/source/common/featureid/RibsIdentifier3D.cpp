/***
 * millipede: RibsIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "RibsIdentifier3D.h"

#include <boost/bind.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
RibsIdentifier3D::RibsIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	StratifiedRegionGrowingFeatureIdentifier(dicomVolume, volumeIPF)
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

	// Step 2: Filter for the rib seed nodes.
	std::list<PFNodeID> seeds = filter_branch_nodes(boost::bind(&RibsIdentifier3D::is_seed, this, _1, _2, spineProperties));

	// Step 3: Grow regions from the seed nodes.
	PartitionForestSelection_Ptr regions = grow_regions(seeds, boost::bind(&RibsIdentifier3D::grow_condition, this, _1, _2, _3, _4, _5));

	// Step 4: (TEMPORARILY) Mark the resulting regions as rib (and unmark them as spine if necessary).
	multiFeatureSelection->identify_selection(regions, AbdominalFeature::RIB);
	multiFeatureSelection->unidentify_selection(regions, AbdominalFeature::VERTEBRA);
}

bool RibsIdentifier3D::grow_condition(const PFNodeID& adj, const BranchProperties& adjProperties, const BranchProperties& curProperties,
									  const BranchProperties& seedProperties, const BranchProperties& overallProperties) const
{
	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();
	return adjProperties.mean_grey_value() >= 180 && !multiFeatureSelection->selection(AbdominalFeature::VERTEBRA)->contains(adj);
}

bool RibsIdentifier3D::is_seed(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties) const
{
	int maxVoxelsPerSlice = 500;
	int sliceCount = properties.z_max() + 1 - properties.z_min();

	return	properties.voxel_count() <= maxVoxelsPerSlice * sliceCount &&	// it should be a relatively small node
			properties.mean_grey_value() >= 200 &&							// it should have a high grey value
			properties.y_max() <= spineProperties.y_max() + 10 &&			// it should not be significantly below the spine (excludes things like the table)
			(properties.x_min() < spineProperties.x_min() ||				// and it should not be contained within the spine in the x axis
			 properties.x_max() > spineProperties.x_max());
}

}
