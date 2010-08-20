/***
 * millipede: KidneysIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "KidneysIdentifier3D.h"

#include <boost/bind.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
KidneysIdentifier3D::KidneysIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	FeatureIdentifier(dicomVolume, volumeIPF)
{}

//#################### PUBLIC METHODS ####################
int KidneysIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void KidneysIdentifier3D::execute_impl()
{
	set_status("Identifying kidneys...");

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	// Step 1: Calculate the combined properties of all the nodes marked as part of the spine.
	BranchProperties spineProperties = multiFeatureSelection->properties_of(AbdominalFeature::VERTEBRA);

	// Step 2: Filter for kidneys.
	std::list<PFNodeID> nodes = filter_branch_nodes(boost::bind(&KidneysIdentifier3D::is_kidney, this, _1, _2, spineProperties));

	// Step 3: (TEMPORARY) Mark the results as kidney (and unmark them as liver if necessary).
	for(std::list<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		multiFeatureSelection->identify_node(*it, AbdominalFeature::KIDNEY);
		multiFeatureSelection->unidentify_node(*it, AbdominalFeature::LIVER);
	}
}

bool KidneysIdentifier3D::is_kidney(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties) const
{
	int minVoxelsPerSlice = 2000;
	int maxVoxelsPerSlice = 6000;
	int sliceCount = properties.z_max() + 1 - properties.z_min();
	int minVoxels = minVoxelsPerSlice * sliceCount;
	int maxVoxels = maxVoxelsPerSlice * sliceCount;
	double aspectRatio = properties.aspect_ratio();

	return	node.layer() >= 3 &&																// it should be in layer 3 or above (lower nodes aren't kidneys)
			160 <= properties.mean_grey_value() && properties.mean_grey_value() <= 190 &&		// it should have a reasonably (but not excessively) high grey value
			(properties.x_max() < spineProperties.centroid().x ||								// it should not cross the spine centroid in the x direction
			 properties.x_min() > spineProperties.centroid().x) &&
			properties.y_max() >= spineProperties.y_min() &&									// it should extend as far back as the spine in the y direction
			0.25 <= aspectRatio && aspectRatio <= 4 &&											// it should have a reasonable aspect ratio
			properties.voxel_count() >= minVoxels && properties.voxel_count() <= maxVoxels;		// it should be a reasonable size
}

}
