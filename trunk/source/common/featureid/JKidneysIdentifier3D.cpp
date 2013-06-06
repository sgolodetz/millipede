/***
 * millipede: JKidneysIdentifier3D.cpp
 * Jess Pumphrey, 2012
 ***/

#include "JKidneysIdentifier3D.h"

#include <boost/bind.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
JKidneysIdentifier3D::JKidneysIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, FIOptions map)
:	FeatureIdentifier(dicomVolume, volumeIPF), m_map(map)
{}

//#################### PUBLIC METHODS ####################
int JKidneysIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void JKidneysIdentifier3D::execute_impl()
{
	set_status("Identifying kidneys...");
	std::cout << "Identifying kidneys" << std::endl;

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();
	
	if (get_multi_feature_selection()->selection(AbdominalFeature::VERTEBRA)->empty()) {
		std::cout << "No spine found" << std::endl;
		return;
	}
	
	if (get_multi_feature_selection()->selection(AbdominalFeature::RIB)->empty()) {
		std::cout << "No ribs found" << std::endl;
		return;
	}
	// Step 1: Calculate the combined properties of all the nodes marked as part of the spine.
	BranchProperties spineProperties = multiFeatureSelection->properties_of(AbdominalFeature::VERTEBRA);
	BranchProperties ribProperties = multiFeatureSelection->properties_of(AbdominalFeature::RIB);

	// Step 2: Filter for kidneys.
	std::list<PFNodeID> nodesL = filter_branch_nodes(boost::bind(&JKidneysIdentifier3D::is_kidneyL, this, _1, _2, spineProperties, ribProperties, m_map));
	std::list<PFNodeID> nodesR = filter_branch_nodes(boost::bind(&JKidneysIdentifier3D::is_kidneyR, this, _1, _2, spineProperties, ribProperties, m_map));
	PartitionForestSelection_Ptr estimateR;
	PartitionForestSelection_Ptr estimateL;
	
	
	// Step 3: (TEMPORARY) Mark the results as kidney (and unmark them as liver if necessary).
	/*for(std::list<PFNodeID>::const_iterator it=nodesL.begin(), iend=nodesL.end(); it!=iend; ++it)
	{
		multiFeatureSelection->identify_node(*it, AbdominalFeature::KIDNEY_LEFT);
		multiFeatureSelection->unidentify_node(*it, AbdominalFeature::LIVER);
	}
	// Step 3: (TEMPORARY) Mark the results as kidney (and unmark them as liver if necessary).
	for(std::list<PFNodeID>::const_iterator it=nodesR.begin(), iend=nodesR.end(); it!=iend; ++it)
	{
		multiFeatureSelection->identify_node(*it, AbdominalFeature::KIDNEY_RIGHT);
		multiFeatureSelection->unidentify_node(*it, AbdominalFeature::LIVER);
	}*/
	
	PFNodeID bestCandidate;
	int bestXMax = INT_MIN;
	
	// Step 3: (TEMPORARY) Mark the results as kidney (and unmark them as liver if necessary).
	for(std::list<PFNodeID>::const_iterator it=nodesR.begin(), iend=nodesR.end(); it!=iend; ++it)
	{
		if (touching_spine(*it, multiFeatureSelection, volume_ipf())) {
			std::cout << *it << " touches the spine" << std::endl;
			continue;
		}
		else {
			std::cout << *it << " doesn't touch the spine" << std::endl;
		}
		
		const BranchProperties& properties = volume_ipf()->branch_properties(*it);
		if(properties.x_max() > bestXMax)
		{
			bestCandidate = *it;
			bestXMax = properties.x_max();
		}
	}
	
	std::cout << "bestCandidate (R) = " << bestCandidate << std::endl;
	
	//multiFeatureSelection->identify_node(bestCandidate, AbdominalFeature::KIDNEY_RIGHT);
		// If we can't find a candidate right kidney, exit.
	if( !(bestCandidate == PFNodeID::invalid())) {

		increment_progress();
		
		// Step 3: Grow a region from the candidate liver.
		std::list<PFNodeID> nodeList;
		nodeList.push_back(bestCandidate);
		
		std::cout << "growing" << std::endl;
		
		estimateR = grow_regions(nodeList, boost::bind(&JKidneysIdentifier3D::grow_condition, this, _1, _2, _3, _4, _5, spineProperties, m_map));
		increment_progress();
		
		//multiFeatureSelection->unidentify_selection(estimateR, AbdominalFeature::LIVER);
		//multiFeatureSelection->identify_selection(estimateR, AbdominalFeature::KIDNEY_RIGHT);
		
		/*
		std::cout << "identifying" << std::endl;
		
		std::set<PFNodeID> regionNodes(estimateR->view_at_layer_cbegin(3), estimateR->view_at_layer_cend(3));
		for(std::set<PFNodeID>::const_iterator it=regionNodes.begin(), iend=regionNodes.end(); it!=iend; ++it)
		{
			
			std::cout << "adding node " << *it << std::endl;
			multiFeatureSelection->unidentify_node(*it, AbdominalFeature::LIVER);
			multiFeatureSelection->identify_node(*it, AbdominalFeature::KIDNEY_RIGHT);
		}*/
		
	}
	
	PFNodeID bestCandidate2;
	std::cout << "finding bestCandidate (L) " << std::endl;
	int bestXMin = INT_MAX;
	
	std::cout << "entering for loop" << std::endl;
	
	// Step 3: (TEMPORARY) Mark the results as kidney (and unmark them as liver if necessary).
	for(std::list<PFNodeID>::const_iterator it=nodesL.begin(), iend=nodesL.end(); it!=iend; ++it)
	{
		if (touching_spine(*it, multiFeatureSelection, volume_ipf())) {
			std::cout << *it << " touches the spine" << std::endl;
			continue;
		}
		else {
			std::cout << *it << " doesn't touch the spine" << std::endl;
		}
		const BranchProperties& properties = volume_ipf()->branch_properties(*it);
		if(properties.x_min() < bestXMin)
		{
			bestCandidate2 = *it;
			bestXMin = properties.x_min();
		}
	}

	std::cout << "bestCandidate (L) = " << bestCandidate2 << std::endl;
	
	//multiFeatureSelection->identify_node(bestCandidate2, AbdominalFeature::KIDNEY_LEFT);
	
	// If we can't find a candidate left kidney, exit.
	if(!(bestCandidate2 == PFNodeID::invalid())) {

		increment_progress();
		
		// Step 3: Grow a region from the candidate left kidney.
		std::list<PFNodeID> nodeList2;
		nodeList2.push_back(bestCandidate2);
		
		std::cout << "growing" << std::endl;
		estimateL = grow_regions(nodeList2, boost::bind(&JKidneysIdentifier3D::grow_condition, this, _1, _2, _3, _4, _5, spineProperties, m_map));
		increment_progress();
		
		std::cout << "identifying" << std::endl;
		
		std::set<PFNodeID> regionNodes2(estimateL->view_at_layer_cbegin(3), estimateL->view_at_layer_cend(3));
		for(std::set<PFNodeID>::const_iterator it=regionNodes2.begin(), iend=regionNodes2.end(); it!=iend; ++it)
		{
			std::cout << "adding node " << *it << std::endl;
			//multiFeatureSelection->identify_node(*it, AbdominalFeature::KIDNEY_LEFT);
		}
		increment_progress();
		
	}
	
	if (!(bestCandidate2 == PFNodeID::invalid())) {
		std::cout << "identifying kidney L" << std::endl;
		multiFeatureSelection->unidentify_selection(estimateL, AbdominalFeature::LIVER);
		multiFeatureSelection->identify_selection(estimateL, AbdominalFeature::KIDNEY_LEFT);
		//multiFeatureSelection->identify_node(bestCandidate2, AbdominalFeature::MARKER);
	}
	
	if (!(bestCandidate == PFNodeID::invalid())) {
		std::cout << "identifying kidney R" << std::endl;
			
		multiFeatureSelection->unidentify_selection(estimateR, AbdominalFeature::LIVER);
		multiFeatureSelection->identify_selection(estimateR, AbdominalFeature::KIDNEY_RIGHT);
		//multiFeatureSelection->identify_node(bestCandidate, AbdominalFeature::MARKER);
	}
	
}

bool JKidneysIdentifier3D::grow_condition(const PFNodeID& adj, const BranchProperties& adjProperties, const BranchProperties& curProperties,
									   const BranchProperties& seedProperties, const BranchProperties& overallProperties, const BranchProperties& spineProperties, FIOptions map) const
{
		std::cout << "checking grow conditions of " << adj << std::endl;
		
		int maxVoxelsPerSlice = map["KMaxVox"];
		int sliceCount = overallProperties.z_max() + 1 - overallProperties.z_min();
		int maxVoxels = maxVoxelsPerSlice * sliceCount;
		
		static std::vector<BranchProperties> componentProperties(2);
		componentProperties[0] = overallProperties;
		componentProperties[1] = adjProperties;
		BranchProperties potentialOverallProperties = BranchProperties::combine_branch_properties(componentProperties);
		
	
		return ((adjProperties.x_max() < seedProperties.x_max() && seedProperties.x_max() < spineProperties.centroid().x) || (adjProperties.x_min() > seedProperties.x_min() &&  seedProperties.x_min() > spineProperties.centroid().x)) &&
		
			map["KMinH"] <= adjProperties.mean_houndsfield_value() && adjProperties.mean_houndsfield_value() <= map["KMaxH"] &&		// it should have a reasonable grey value
			
			((map["KMinAspect"] / 10) < potentialOverallProperties.aspect_ratio_xy() && potentialOverallProperties.aspect_ratio_xy() < (map["KMaxAspect"] / 10)) &&
			
			potentialOverallProperties.voxel_count() < maxVoxels &&
			
			potentialOverallProperties.centroid().y > spineProperties.y_min() &&
			
			
			
			fabs(adjProperties.mean_houndsfield_value() - seedProperties.mean_houndsfield_value()) < map["KSeedTolerance"] &&		// it should have roughly the same grey value as the seed
			fabs(adjProperties.mean_houndsfield_value() - curProperties.mean_houndsfield_value()) < map["KAdjTolerance"];			// it should have roughly the same grey value as its generating region
}


bool JKidneysIdentifier3D::touching_spine(const PFNodeID& node, VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection, VolumeIPF_Ptr volumeIPF) const {
	
	//std::cout << "touching_spine" << std::endl;
	
	std::vector<int> adjNodes = volumeIPF->adjacent_nodes(node);
	//std::cout << "before for" << std::endl;
	for(std::vector<int>::const_iterator it=adjNodes.begin(), iend=adjNodes.end(); it!=iend; ++it)
	{
		//std::cout << "in for" << std::endl;
		PFNodeID adj(node.layer(), *it);
		//std::cout << "before if" << std::endl;
		
		if (multiFeatureSelection->selection(AbdominalFeature::VERTEBRA)->contains(adj)) {
		//	std::cout << "in if" << std::endl;
			return true;
		}
		//std::cout << "after if" << std::endl;
	}
	//std::cout << "returning" << std::endl;

	return false;
	
}

bool JKidneysIdentifier3D::is_kidneyR(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, const BranchProperties& ribProperties, FIOptions map) const
{
	int minVoxelsPerSlice = map["KMinVox"];
	int maxVoxelsPerSlice = map["KMaxVox"];
	int sliceCount = properties.z_max() + 1 - properties.z_min();
	int minVoxels = minVoxelsPerSlice * sliceCount;
	int maxVoxels = maxVoxelsPerSlice * sliceCount;
	double aspectRatioXY = properties.aspect_ratio_xy();
	
	return	node.layer() >= 3 &&																// it should be in layer 3 or above (lower nodes aren't kidneys)
			map["KMinH"] <= properties.mean_houndsfield_value() && properties.mean_houndsfield_value() <= map["KMaxH"] &&		// it should have a reasonably (but not excessively) high grey value &&
			(properties.x_min() > (spineProperties.x_min() - map["KSpineClose"]) ||
			 properties.x_max() < (spineProperties.x_max() + map["KSpineClose"])) && //should still be reasonably close to the spine 
			 (properties.x_min() > (ribProperties.x_min() + map["KRibDist"]) &&
			 properties.x_max() < (ribProperties.x_max() - map["KRibDist"])) && //shouldn't be too close to the outside of the body
			 
			(properties.x_max() < spineProperties.centroid().x ) && //to the left of the spine centroid
			properties.centroid()[1] >= (spineProperties.y_min() - 30) &&									// it should extend as far back as the spine in the y direction		
			properties.centroid()[1] <= (spineProperties.y_max() - 20) && //but not much further
			minVoxels <= properties.voxel_count() && properties.voxel_count() <= maxVoxels;		// it should be a reasonable size
}
bool JKidneysIdentifier3D::is_kidneyL(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, const BranchProperties& ribProperties, FIOptions map) const
{
	int minVoxelsPerSlice = map["KMinVox"];
	int maxVoxelsPerSlice = map["KMaxVox"];
	int sliceCount = properties.z_max() + 1 - properties.z_min();
	int minVoxels = minVoxelsPerSlice * sliceCount;
	int maxVoxels = maxVoxelsPerSlice * sliceCount;
	double aspectRatioXY = properties.aspect_ratio_xy();

	return	node.layer() >= 3 &&																// it should be in layer 3 or above (lower nodes aren't kidneys)
			map["KMinH"] <= properties.mean_houndsfield_value() && properties.mean_houndsfield_value() <= map["KMaxH"] &&		// it should have a reasonably (but not excessively) high grey value &&
			(properties.x_min() > (spineProperties.x_min() - map["KSpineClose"]) ||
			 properties.x_max() < (spineProperties.x_max() + map["KSpineClose"])) && //should still be reasonably close to the spine 
			 (properties.x_min() > (ribProperties.x_min() + map["KRibDist"]) &&
			 properties.x_max() < (ribProperties.x_max() - map["KRibDist"])) && //shouldn't be too close to the outside of the body
			 
			(properties.x_min() > spineProperties.centroid().x) && //to the right of the spine centroid
			properties.centroid()[1] >= (spineProperties.y_min() - 30) &&									// it should extend as far back as the spine in the y direction
			properties.centroid()[1] <= (spineProperties.y_max() - 20) && //but not much further
			minVoxels <= properties.voxel_count() && properties.voxel_count() <= maxVoxels;		// it should be a reasonable size
}

}
