/***
 * millipede: SpinalCordIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SpinalCordIdentifier3D.h"

#include <boost/bind.hpp>
#include <iostream>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### CONSTRUCTORS ####################
SpinalCordIdentifier3D::SpinalCordIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	FeatureIdentifier(dicomVolume, volumeIPF)
{}

//#################### PUBLIC METHODS ####################
int SpinalCordIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void SpinalCordIdentifier3D::execute_impl()
{
	set_status("Identifying spinal cord...");

	std::cout << "Identifying the spinal cord..." << std::endl;
	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	std::cout << "getting spine porperties" << std::endl;
	// Step 1: Calculate the combined properties of all the nodes marked as part of the spine.
	BranchProperties spineProperties = multiFeatureSelection->properties_of(AbdominalFeature::VERTEBRA);

	std::cout << "filtering nodes" << std::endl;
	// Step 2: Filter for spinal cord nodes based on the location of the spine.
	std::list<PFNodeID> nodes = filter_branch_nodes(boost::bind(&SpinalCordIdentifier3D::is_spinal_cord, this, _1, _2, spineProperties));

	std::cout << "idenitfying nodes" << std::endl;
	// Step 3: Mark the resulting nodes as spinal cord (and unmark them as spine if necessary).
	for(std::list<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		multiFeatureSelection->identify_node(*it, AbdominalFeature::SPINAL_CORD);
		multiFeatureSelection->unidentify_node(*it, AbdominalFeature::VERTEBRA);
	}
}

bool SpinalCordIdentifier3D::is_spinal_cord(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties) const
{
	itk::Index<3> volumeSize = ITKImageUtil::make_index_from_size(dicom_volume()->size());
	int minVoxels = 300 * volumeSize[2];
	int maxVoxels = 1000 * volumeSize[2];
	const int X_TOLERANCE = -5;
	const int Y_TOLERANCE = -10;

	return	properties.x_min() >= spineProperties.x_min() - X_TOLERANCE &&						// it should be well within the spine in the x direction
			properties.x_max() <= spineProperties.x_max() + X_TOLERANCE &&
			properties.x_min() < spineProperties.centroid().x &&								// it should straddle the spine's centroid in the x direction
			properties.x_max() > spineProperties.centroid().x &&
			properties.y_min() >= spineProperties.y_min() - Y_TOLERANCE &&						// it should be well within the spine in the y direction
			properties.y_max() <= spineProperties.y_max() + Y_TOLERANCE &&
			properties.z_min() == 0 && properties.z_max() == volumeSize[2]-1 &&					// it should extend through all the slices we're looking at
			properties.mean_grey_value() <= 140 &&												// it should have a reasonably low grey value
			properties.voxel_count() >= minVoxels && properties.voxel_count() <= maxVoxels;		// and a reasonable size
}

}
