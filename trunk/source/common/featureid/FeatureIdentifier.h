/***
 * millipede: FeatureIdentifier.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FEATUREIDENTIFIER
#define H_MILLIPEDE_FEATUREIDENTIFIER

#ifdef _MSC_VER
	// Disable the spurious "inherits * via dominance" warnings that VC++ will throw out - the virtual inheritance being used here is entirely intentional.
	#pragma warning(disable:4250)
#endif

#include <common/jobs/DataHook.h>
#include <common/jobs/Job.h>
#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

class FeatureIdentifier : public virtual Job
{
	//#################### TYPEDEFS ####################
protected:
	typedef DICOMImageBranchLayer BranchLayer;
	typedef AbdominalFeature::Enum Feature;
	typedef DICOMImageLeafLayer LeafLayer;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;
	typedef PartitionForestSelection<LeafLayer,BranchLayer> PartitionForestSelectionT;
	typedef boost::shared_ptr<PartitionForestSelectionT> PartitionForestSelection_Ptr;
	typedef VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;

	typedef VolumeIPFT::BranchNodeConstIterator BranchNodeConstIterator;
	typedef VolumeIPFT::BranchProperties BranchProperties;
	typedef VolumeIPFT::LeafNodeConstIterator LeafNodeConstIterator;
	typedef VolumeIPFT::LeafProperties LeafProperties;

	//#################### PRIVATE VARIABLES ####################
private:
	DICOMVolume_CPtr m_dicomVolume;
	DataHook<VolumeIPFMultiFeatureSelection_Ptr> m_mfsHook;
	VolumeIPF_Ptr m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	FeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& get_mfs_hook() const;
	const VolumeIPFMultiFeatureSelection_Ptr& get_multi_feature_selection() const;
	void set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook);

	//#################### PROTECTED METHODS ####################
protected:
	DICOMVolume_CPtr dicom_volume() const;
	void morphologically_close_nodes(std::set<PFNodeID>& nodes, int n = 1) const;
	void morphologically_dilate_nodes(std::set<PFNodeID>& nodes, int n = 1) const;
	void morphologically_erode_nodes(std::set<PFNodeID>& nodes, int n = 1) const;
	VolumeIPF_Ptr volume_ipf() const;

	template <typename Pred>
	std::list<PFNodeID> filter_branch_nodes(Pred pred) const
	{
		std::list<PFNodeID> result;
		for(int layer=1, highestLayer=m_volumeIPF->highest_layer(); layer<=highestLayer; ++layer)
		{
			for(BranchNodeConstIterator it=m_volumeIPF->branch_nodes_cbegin(layer), iend=m_volumeIPF->branch_nodes_cend(layer); it!=iend; ++it)
			{
				PFNodeID node(layer, it.index());
				if(pred(node, it->properties()))
				{
					result.push_back(node);
				}
			}
		}
		return result;
	}

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
