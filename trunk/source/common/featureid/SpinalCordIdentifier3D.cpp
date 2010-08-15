/***
 * millipede: SpinalCordIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SpinalCordIdentifier3D.h"

#include <boost/bind.hpp>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/util/ITKImageUtil.h>
#include "FeatureIdentificationUtil.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SpinalCordIdentifier3D::SpinalCordIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	m_dicomVolume(dicomVolume), m_volumeIPF(volumeIPF)
{}

//#################### PUBLIC METHODS ####################
const DataHook<SpinalCordIdentifier3D::VolumeIPFMultiFeatureSelection_Ptr>& SpinalCordIdentifier3D::get_mfs_hook() const
{
	return m_mfsHook;
}

const SpinalCordIdentifier3D::VolumeIPFMultiFeatureSelection_Ptr& SpinalCordIdentifier3D::get_output() const
{
	return m_mfsHook.get();
}

int SpinalCordIdentifier3D::length() const
{
	return 1;
}

void SpinalCordIdentifier3D::set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook)
{
	m_mfsHook = mfsHook;
}

//#################### PRIVATE METHODS ####################
void SpinalCordIdentifier3D::execute_impl()
{
	set_status("Identifying spinal cord...");

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = m_mfsHook.get();

	// Step 1: Calculate the combined properties of all the nodes marked as part of the spine.
	PartitionForestSelection_CPtr spineSelection = multiFeatureSelection->selection(AbdominalFeature::VERTEBRA);
	std::vector<BranchProperties> spineComponentProperties;
	for(PartitionForestSelectionT::NodeConstIterator it=spineSelection->nodes_cbegin(), iend=spineSelection->nodes_cend(); it!=iend; ++it)
	{
		if(it->layer() != 0)
		{
			spineComponentProperties.push_back(m_volumeIPF->branch_properties(*it));
		}
		else
		{
			std::vector<std::pair<Vector3i,LeafProperties> > leafProperties;
			itk::Index<3> p = m_volumeIPF->position_of_leaf(it->index());
			leafProperties.push_back(std::make_pair(Vector3i(p[0], p[1], p[2]), m_volumeIPF->leaf_properties(it->index())));
			spineComponentProperties.push_back(BranchProperties::combine_leaf_properties(leafProperties));
		}
	}
	BranchProperties spineProperties = BranchProperties::combine_branch_properties(spineComponentProperties);

	// Step 2: Filter for spinal cord nodes based on the location of the spine.
	std::list<PFNodeID> nodes = FeatureIdentificationUtil::filter_branch_nodes(m_volumeIPF, boost::bind(&SpinalCordIdentifier3D::is_spinal_cord, this, _1, spineProperties));

	// Step 3: Mark the resulting nodes as spinal cord (and unmark them as spine if necessary).
	for(std::list<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		multiFeatureSelection->identify_node(*it, AbdominalFeature::SPINAL_CORD);
		multiFeatureSelection->unidentify_node(*it, AbdominalFeature::VERTEBRA);
	}
}

bool SpinalCordIdentifier3D::is_spinal_cord(const BranchProperties& properties, const BranchProperties& spineProperties) const
{
	itk::Index<3> volumeSize = ITKImageUtil::make_index_from_size(m_dicomVolume->size());
	int minVoxels = 450 * volumeSize[2];
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
