/***
 * millipede: JRibsIdentifier3D.cpp
 * Jess Pumphrey, 2012
 ***/

#include "JRibsIdentifier3D.h"

#include <boost/bind.hpp>
#include <common/math/Vector3.h>
#include <iostream>

namespace mp {

//#################### CONSTRUCTORS ####################
JRibsIdentifier3D::JRibsIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, FIOptions map)
:	FeatureIdentifier(dicomVolume, volumeIPF), m_map(map)
{}

//#################### PUBLIC METHODS ####################
int JRibsIdentifier3D::length() const
{
	return 5;
}

//#################### PRIVATE METHODS ####################
void JRibsIdentifier3D::execute_impl()
{
	set_status("Identifying ribs...");
	std::cout << "Identifying ribs" << std::endl;

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	if (multiFeatureSelection->selection(AbdominalFeature::VERTEBRA)->empty()) {
		std::cout << "No spine found" << std::endl;
		return;
	}
	// Step 1: Calculate the combined properties of all the nodes marked as part of the spine.
	BranchProperties spineProperties = multiFeatureSelection->properties_of(AbdominalFeature::VERTEBRA);
	increment_progress();

	// Step 2: Filter for the rib seed nodes.
	std::list<PFNodeID> seeds = filter_branch_nodes(boost::bind(&JRibsIdentifier3D::is_seed, this, _1, _2, spineProperties, m_map));
	increment_progress();

	std::cout << "seeds.size()" << seeds.size() << std::endl;
	
	// Step 3: Grow regions from the seed nodes.
	PartitionForestSelection_Ptr preliminaryRegions = grow_regions(seeds, boost::bind(&JRibsIdentifier3D::grow_condition, this, _1, _2, _3, _4, _5, m_map));
	increment_progress();

	// Step 4: Post-process the regions to get rid of anything undesirable.
	PartitionForestSelection_Ptr finalRegions = postprocess_regions(preliminaryRegions, spineProperties, m_map);
	increment_progress();

	// Step 5: Mark the final regions as rib (and unmark them as spine if necessary).
	multiFeatureSelection->identify_selection(finalRegions, AbdominalFeature::RIB);
	multiFeatureSelection->unidentify_selection(finalRegions, AbdominalFeature::VERTEBRA);
	std::cout << "seeds.size()" << seeds.size() << std::endl;
}

bool JRibsIdentifier3D::grow_condition(const PFNodeID& adj, const BranchProperties& adjProperties, const BranchProperties& curProperties,
									  const BranchProperties& seedProperties, const BranchProperties& overallProperties, FIOptions map) const
{
//	return false;
	//std::cout << "growing" << std::endl;
	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();
	//std::cout << "growing " << adjProperties.mean_houndsfield_value() << " -- " << map["RMinGrowH"] << std::endl;
	return adjProperties.mean_houndsfield_value() >= map["RMinGrowH"] && !multiFeatureSelection->selection(AbdominalFeature::VERTEBRA)->contains(adj);
}

bool JRibsIdentifier3D::is_seed(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, FIOptions map) const
{
	int maxVoxelsPerSlice = map["RMaxVox"];
	int sliceCount = properties.z_max() + 1 - properties.z_min();

	return	properties.voxel_count() <= maxVoxelsPerSlice * sliceCount &&	// it should be a relatively small node
			properties.mean_houndsfield_value() >= map["RMinSeedH"] &&							// it should have a high grey value
			properties.y_max() <= spineProperties.y_max() + 10 &&			// it should not be significantly below the spine (excludes things like the table)
			(properties.x_min() < spineProperties.x_min() ||				// and it should not be contained within the spine in the x axis
			 properties.x_max() > spineProperties.x_max());
}

bool JRibsIdentifier3D::good_position(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, FIOptions map) const
{

	//std::cout << "Checking positioning of " << properties.centroid() << std::endl;
	
	Vector3d bodyCentre = Vector3d(spineProperties.centroid()[0],
				       spineProperties.centroid()[1] - spineProperties.y_max() + spineProperties.y_min(),
				       properties.centroid()[2]);
	
	Vector3d radius = bodyCentre - properties.centroid();
	
	//std::cout << "bodyCentre = " << bodyCentre << ", radius = " << radius << std::endl;
	
	double scaleFactor = (0.0 + map["RBoundaryScale"]) / 10;
	
	radius = (bodyCentre - (radius * scaleFactor)) + Vector3d(0,0,radius[2] * scaleFactor);
	
	//std::cout << "new radius = " << radius << std::endl;
	
	itk::Index<3> outside;
	for(int i=0; i<3; ++i) outside[i] = NumericUtil::round_to_nearest<int>(radius[i]);
	
	
	//std::cout << "outside = " << outside << std::endl;
	
	PFNodeID outsideNode = volume_ipf()->node_of(1, outside);
	
	
	//std::cout << "this node = " << node << std::endl;
	//std::cout << "outsideNode = " << outsideNode << std::endl;
	
	if (outsideNode == PFNodeID::invalid()) {
		
	//	std::cout << "Node out of bounds" << std::endl;
		return true;
	}
	
	std::cout << volume_ipf()->branch_properties(outsideNode).mean_houndsfield_value() << std::endl;
	
	if (volume_ipf()->branch_properties(outsideNode).mean_houndsfield_value() < map["OutsideMaxH"]) {
		
	//	std::cout << "Outside node is indeed outside" << std::endl;
		return true;
	}
	
	//std::cout << "Outside node is not outside" << std::endl;
	return false;
}

JRibsIdentifier3D::PartitionForestSelection_Ptr JRibsIdentifier3D::postprocess_regions(const PartitionForestSelection_Ptr& preliminaryRegions, const BranchProperties& spineProperties, FIOptions map) const
{
	PartitionForestSelection_Ptr finalRegions(new PartitionForestSelectionT(volume_ipf()));

	// Step 1: Build the set of preliminary regions at the level of their merge layer.
	int mergeLayer;
	std::set<int> indices;
	boost::tie(mergeLayer, indices) = extract_merge_layer_indices(preliminaryRegions, volume_ipf()->highest_layer());

	// Step 2a: Remove any darker regions.
	for(std::set<int>::iterator it=indices.begin(), iend=indices.end(); it!=iend;)
	{
		PFNodeID node(mergeLayer, *it);
		const BranchProperties& properties = volume_ipf()->branch_properties(node);
		if(properties.mean_houndsfield_value() < map["RPPMinH"]) indices.erase(it++);
		else ++it;
	}
	
	// Step 2b: Remove any regions which are too far from the outside of the body
	for(std::set<int>::iterator it=indices.begin(), iend=indices.end(); it!=iend;)
	{
		PFNodeID node(mergeLayer, *it);
		const BranchProperties& properties = volume_ipf()->branch_properties(node);
		if(!good_position(node, properties, spineProperties, map)) {
			//std::cout << "Removing " << properties.centroid() << " as it's too far into the body" << std::endl;
			indices.erase(it++);
		}
		else ++it;
	}

	// Step 3: Find the connected components of what remains.
	std::vector<std::set<int> > connectedComponents = volume_ipf()->find_connected_components(indices, mergeLayer);

	// Step 4:	Either keep or discard each connected component based on its properties.
	int minVoxelsPerSlice = map["RPPMinVox"];
	int maxVoxelsPerSlice = map["RPPMaxVox"];
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
		//	std::cout << node << " IS A RIB" << std::endl;
		}
	}

	return finalRegions;
}

}
