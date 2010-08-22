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
	return 5;
}

//#################### PRIVATE METHODS ####################
void RibsIdentifier3D::execute_impl()
{
	set_status("Identifying ribs...");

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	// Step 1: Calculate the combined properties of all the nodes marked as part of the spine.
	BranchProperties spineProperties = multiFeatureSelection->properties_of(AbdominalFeature::VERTEBRA);
	increment_progress();

	// Step 2: Filter for the rib seed nodes.
	std::list<PFNodeID> seeds = filter_branch_nodes(boost::bind(&RibsIdentifier3D::is_seed, this, _1, _2, spineProperties));
	increment_progress();

	// Step 3: Grow regions from the seed nodes.
	PartitionForestSelection_Ptr preliminaryRegions = grow_regions(seeds, boost::bind(&RibsIdentifier3D::grow_condition, this, _1, _2, _3, _4, _5));
	increment_progress();

	// Step 4: Post-process the regions to get rid of anything undesirable.
	PartitionForestSelection_Ptr finalRegions = postprocess_regions(preliminaryRegions);
	increment_progress();

	// Step 5: Mark the final regions as rib (and unmark them as spine if necessary).
	multiFeatureSelection->identify_selection(finalRegions, AbdominalFeature::RIB);
	multiFeatureSelection->unidentify_selection(finalRegions, AbdominalFeature::VERTEBRA);
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

RibsIdentifier3D::PartitionForestSelection_Ptr RibsIdentifier3D::postprocess_regions(const PartitionForestSelection_Ptr& preliminaryRegions) const
{
	PartitionForestSelection_Ptr finalRegions(new PartitionForestSelectionT(volume_ipf()));

	// Step 1: Build the set of preliminary regions at the level of their merge layer.
	int mergeLayer;
	std::set<int> indices;
	boost::tie(mergeLayer, indices) = extract_merge_layer_indices(preliminaryRegions, volume_ipf()->highest_layer());

	// Step 2: Remove any darker regions.
	for(std::set<int>::iterator it=indices.begin(), iend=indices.end(); it!=iend;)
	{
		PFNodeID node(mergeLayer, *it);
		const BranchProperties& properties = volume_ipf()->branch_properties(node);
		if(properties.mean_grey_value() < 150) indices.erase(it++);
		else ++it;
	}

	// Step 3: Find the connected components of what remains.
	std::vector<std::set<int> > connectedComponents = volume_ipf()->find_connected_components(indices, mergeLayer);

	// Step 4:	Either keep or discard each connected component based on its properties.
	int minVoxelsPerSlice = 90;
	int maxVoxelsPerSlice = 500;
	for(std::vector<std::set<int> >::iterator it=connectedComponents.begin(), iend=connectedComponents.end(); it!=iend; ++it)
	{
		// Calculate the properties of the connected component.
		BranchProperties componentProperties = calculate_component_properties(mergeLayer, *it);

		// Decide whether or not to discard the connected component.
		int sliceCount = componentProperties.z_max() + 1 - componentProperties.z_min();
		if(componentProperties.voxel_count() < minVoxelsPerSlice * sliceCount || componentProperties.voxel_count() > maxVoxelsPerSlice * sliceCount)
		{
			continue;
		}

		// Add any surviving connected component to the final regions.
		for(std::set<int>::const_iterator jt=it->begin(), jend=it->end(); jt!=jend; ++jt)
		{
			PFNodeID node(mergeLayer, *jt);
			finalRegions->select_node(node);
		}
	}

	return finalRegions;
}

}
