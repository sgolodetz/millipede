/***
 * millipede: AortaIdentifier3D.cpp
 * Jess Pumphrey, 2012
 ***/

#include "JAortaIdentifier3D.h"

#include <cassert>

#include <boost/bind.hpp>
#include <boost/tuple/tuple.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
JAortaIdentifier3D::JAortaIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, FIOptions map)
:	FeatureIdentifier(dicomVolume, volumeIPF), m_map(map)
{}

//#################### PUBLIC METHODS ####################
int JAortaIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void JAortaIdentifier3D::execute_impl()
{
	set_status("Identifying aorta...");
	std::cout << "Identifying aorta" << std::endl;

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	if (get_multi_feature_selection()->selection(AbdominalFeature::VERTEBRA)->empty()) {
		std::cout << "No spine found" << std::endl;
		return;
	}
	
	if (get_multi_feature_selection()->selection(AbdominalFeature::SPINAL_CORD)->empty()) {
		std::cout << "No spinal cord found" << std::endl;
		return;
	}
	
	// Step 1: Calculate the combined properties of all the nodes marked as part of the spine and spinal cord.
	BranchProperties spineProperties = multiFeatureSelection->properties_of(AbdominalFeature::VERTEBRA);
	BranchProperties spinalCordProperties = multiFeatureSelection->properties_of(AbdominalFeature::SPINAL_CORD);

	// Step 2: Filter for aorta-like regions.
	std::list<PFNodeID> seeds = filter_branch_nodes(boost::bind(&JAortaIdentifier3D::is_seed, this, _1, _2, spineProperties, spinalCordProperties, m_map));
	if(seeds.empty()) return;

	PartitionForestSelection_Ptr regions(new PartitionForestSelectionT(volume_ipf()));
	for(std::list<PFNodeID>::const_iterator it=seeds.begin(), iend=seeds.end(); it!=iend; ++it)
	{
		regions->select_node(*it);
	}

	// Step 3: Find the connected components of the results and keep only the one with the greatest yMax.
	int mergeLayer;
	std::set<int> indices;
	boost::tie(mergeLayer, indices) = extract_merge_layer_indices(regions, volume_ipf()->highest_layer());
	std::vector<std::set<int> > connectedComponents = volume_ipf()->find_connected_components(indices, mergeLayer);

	int bestComponentIndex = -1;
	int bestYMax = INT_MIN;
	for(int i=0, size=static_cast<int>(connectedComponents.size()); i<size; ++i)
	{
		BranchProperties componentProperties = calculate_component_properties(mergeLayer, connectedComponents[i]);
		if(componentProperties.y_max() > bestYMax)
		{
			bestComponentIndex = i;
			bestYMax = componentProperties.y_max();
		}
	}

	assert(bestComponentIndex != -1);
	const std::set<int>& bestComponent = connectedComponents[bestComponentIndex];

	// Step 4: Remove any particularly dark regions.
	std::set<PFNodeID> nodes;
	for(std::set<int>::const_iterator it=bestComponent.begin(), iend=bestComponent.end(); it!=iend; ++it)
	{
		PFNodeID node(mergeLayer, *it);
		BranchProperties properties = volume_ipf()->branch_properties(node);
		if(true)//properties.mean_houndsfield_value() >= 130)
		{
			nodes.insert(node);
		}
	}

	// Step 5: Morphologically close what's left to fill in any holes.
	morphologically_close_nodes(nodes, boost::bind(&JAortaIdentifier3D::morphological_condition, this, _1, m_map));

	// Step 6: Mark the resulting nodes as aorta (provided they don't overlap the spine).
	for(std::set<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		if(!multiFeatureSelection->selection(AbdominalFeature::VERTEBRA)->contains(*it))
		{
			multiFeatureSelection->identify_node(*it, AbdominalFeature::AORTA);
		}
	}
}

bool JAortaIdentifier3D::is_seed(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties,
								const BranchProperties& spinalCordProperties, FIOptions map) const
{
	int sliceCount = properties.z_max() + 1 - properties.z_min();
	int minVoxels = map["AMinVox"] * sliceCount;
	int maxVoxels = map["AMaxVox"] * sliceCount;

	return	map["AMinH"] <= properties.mean_houndsfield_value() && properties.mean_houndsfield_value() <= map["AMaxH"] &&								// it should have a reasonable grey value
			minVoxels <= properties.voxel_count() && properties.voxel_count() <= maxVoxels &&							// it should have a reasonable size
			spinalCordProperties.x_min() < properties.x_min() && properties.x_min() < spinalCordProperties.x_max() &&	// its left-hand side should be within the spinal cord bounds in x
			properties.y_min() < spineProperties.y_min() &&																// its top should be above the spine
			properties.y_max() > spineProperties.y_min() - 20;															// its bottom should not be much higher than the top of the spine
}

bool JAortaIdentifier3D::morphological_condition(const BranchProperties& properties, FIOptions map) const
{
	return map["AMinH"] <= properties.mean_houndsfield_value() && properties.mean_houndsfield_value() <= map["AMaxH"];
}

}
