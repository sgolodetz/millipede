/***
 * millipede: JLiverIdentifier3D.cpp
 * Jess Pumphrey, 2012
 ***/

#include "JLiverIdentifier3D.h"

#include <climits>

#include <boost/bind.hpp>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### CONSTRUCTORS ####################
JLiverIdentifier3D::JLiverIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, FIOptions map)
:	FeatureIdentifier(dicomVolume, volumeIPF), m_map(map)
{}

//#################### PUBLIC METHODS ####################
int JLiverIdentifier3D::length() const
{
	return 5;
}

//#################### PRIVATE METHODS ####################
void JLiverIdentifier3D::execute_impl()
{
	set_status("Identifying liver...");
	std::cout << "Identifying liver" << std::endl;

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection = get_multi_feature_selection();

	if (get_multi_feature_selection()->selection(AbdominalFeature::RIB)->empty()) {
		std::cout << "No ribs found" << std::endl;
		return;
	}
	
	BranchProperties ribProperties = multiFeatureSelection->properties_of(AbdominalFeature::RIB);
	
	// Step 1: Filter for the liver candidates.
	std::list<PFNodeID> candidates = filter_branch_nodes(boost::bind(&JLiverIdentifier3D::is_candidate, this, _1, _2, ribProperties, m_map));
	increment_progress();

	// Step 2: Pick the best candidate (namely, the one which stretches furthest to the left of the image).
	PFNodeID bestCandidate;
	int bestXMin = INT_MAX;
	for(std::list<PFNodeID>::const_iterator it=candidates.begin(), iend=candidates.end(); it!=iend; ++it)
	{
		const BranchProperties& properties = volume_ipf()->branch_properties(*it);
		if(properties.x_min() < bestXMin)
		{
			bestCandidate = *it;
			bestXMin = properties.x_min();
		}
	}

	// If we can't find a candidate liver, exit.
	if(bestCandidate == PFNodeID::invalid()) return;

	increment_progress();

	// Step 3: Grow a region from the candidate liver.
	std::list<PFNodeID> bestCandidateList;
	bestCandidateList.push_back(bestCandidate);
	PartitionForestSelection_Ptr region = grow_regions(bestCandidateList, boost::bind(&JLiverIdentifier3D::grow_condition, this, _1, _2, _3, _4, _5, m_map));
	increment_progress();

	// Step 4: Use conditional morphological closing to try and fill any holes.
	std::set<PFNodeID> nodes(region->view_at_layer_cbegin(1), region->view_at_layer_cend(1));
	morphologically_close_nodes(nodes, boost::bind(&JLiverIdentifier3D::morphological_condition, this, _1, m_map));
	PartitionForestSelection_Ptr filledRegion(new PartitionForestSelectionT(volume_ipf()));
	for(std::set<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		filledRegion->select_node(*it);
	}
	increment_progress();

	// Step 5: Mark the result as liver.
	multiFeatureSelection->identify_selection(filledRegion, AbdominalFeature::LIVER);
}

bool JLiverIdentifier3D::grow_condition(const PFNodeID& adj, const BranchProperties& adjProperties, const BranchProperties& curProperties,
									   const BranchProperties& seedProperties, const BranchProperties& overallProperties, FIOptions map) const
{
	return	adjProperties.x_min() >= seedProperties.x_min() &&										// it should be no further left than the original seed
			map["LMinH"] <= adjProperties.mean_houndsfield_value() && adjProperties.mean_houndsfield_value() <= map["LMaxH"] &&		// it should have a reasonable grey value
			fabs(adjProperties.mean_houndsfield_value() - seedProperties.mean_houndsfield_value()) < map["LSeedTolerance"] &&		// it should have roughly the same grey value as the seed
			fabs(adjProperties.mean_houndsfield_value() - curProperties.mean_houndsfield_value()) < map["LAdjTolerance"];			// it should have roughly the same grey value as its generating region
}

bool JLiverIdentifier3D::is_candidate(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& ribProperties, FIOptions map) const
{
	itk::Index<3> volumeSize = ITKImageUtil::make_index_from_size(dicom_volume()->size());
	int minVoxelsPerSlice = map["LMinVox"];
	int sliceCount = properties.z_max() + 1 - properties.z_min();

	return	//node.layer() == 1 &&															// it should be in the lowest branch layer (higher nodes generally stretch too far)
			 (properties.x_min() > (ribProperties.x_min() + map["LRibDistance"]) &&
			 properties.x_max() < (ribProperties.x_max() - map["LRibDistance"])) && //shouldn't be too close to the outside of the body
			properties.voxel_count() >= minVoxelsPerSlice * sliceCount &&					// it should be reasonably-sized
			map["LMinH"] <= properties.mean_houndsfield_value() && properties.mean_houndsfield_value() <= map["LMaxH"] &&	// it should have a reasonable grey value
			properties.x_min() < volumeSize[0]/2;											// and at least part of it should be on the left-hand side of the image
}

bool JLiverIdentifier3D::morphological_condition(const BranchProperties& properties, FIOptions map) const
{
	return properties.mean_grey_value() >= map["LMorphMinH"];
}

}
