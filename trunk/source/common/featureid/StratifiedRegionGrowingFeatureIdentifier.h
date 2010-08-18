/***
 * millipede: StratifiedRegionGrowingFeatureIdentifier.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_STRATIFIEDREGIONGROWINGFEATUREIDENTIFIER
#define H_MILLIPEDE_STRATIFIEDREGIONGROWINGFEATUREIDENTIFIER

#include "FeatureIdentifier.h"

namespace mp {

class StratifiedRegionGrowingFeatureIdentifier : public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	StratifiedRegionGrowingFeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
	:	FeatureIdentifier(dicomVolume, volumeIPF)
	{}

	//#################### PROTECTED METHODS ####################
protected:
	template <typename GrowCondition>
	PartitionForestSelection_Ptr grow_regions(const std::list<PFNodeID>& seeds, GrowCondition growCondition) const
	{
		PartitionForestSelection_Ptr result(new PartitionForestSelectionT(volume_ipf()));
		for(std::list<PFNodeID>::const_iterator it=seeds.begin(), iend=seeds.end(); it!=iend; ++it)
		{
			std::set<PFNodeID> region = grow_region(*it, growCondition);
			for(std::set<PFNodeID>::const_iterator jt=region.begin(), jend=region.end(); jt!=jend; ++jt)
			{
				result->select_node(*jt);
			}
		}
		return result;
	}

	//#################### PRIVATE METHODS ####################
private:
	template <typename GrowCondition>
	std::set<PFNodeID> grow_region(const PFNodeID& seed, GrowCondition growCondition) const
	{
		std::set<PFNodeID> region;
		std::set<int> seen;
		region.insert(seed);
		seen.insert(seed.index());
		BranchProperties seedProperties = volume_ipf()->branch_properties(seed);
		BranchProperties overallProperties = seedProperties;
		grow_region_from(seed, region, growCondition, seen, seedProperties, overallProperties);
		return region;
	}

	template <typename GrowCondition>
	void grow_region_from(const PFNodeID& cur, std::set<PFNodeID>& region, GrowCondition growCondition, std::set<int>& seen,
						  const BranchProperties& seedProperties, BranchProperties& overallProperties) const
	{
		const BranchProperties& curProperties = volume_ipf()->branch_properties(cur);
		static std::vector<BranchProperties> componentProperties(2);

		std::vector<int> adjNodes = volume_ipf()->adjacent_nodes(cur);
		for(std::vector<int>::const_iterator it=adjNodes.begin(), iend=adjNodes.end(); it!=iend; ++it)
		{
			if(seen.find(*it) != seen.end()) continue;	// skip the node if we've already seen it

			seen.insert(*it);
			PFNodeID adj(cur.layer(), *it);
			const BranchProperties& adjProperties = volume_ipf()->branch_properties(adj);

			if(growCondition(adj, adjProperties, curProperties, seedProperties, overallProperties))
			{
				region.insert(adj);
				
				componentProperties[0] = overallProperties;
				componentProperties[1] = adjProperties;
				overallProperties = BranchProperties::combine_branch_properties(componentProperties);

				grow_region_from(adj, region, growCondition, seen, seedProperties, overallProperties);
			}
		}
	}
};

}

#endif
