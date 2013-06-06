/***
 * millipede: VolumeIPFMultiFeatureSelection.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMEIPFMULTIFEATURESELECTION
#define H_MILLIPEDE_VOLUMEIPFMULTIFEATURESELECTION

#include <common/partitionforests/base/PartitionForestMultiFeatureSelection.h>
#include "VolumeIPF.h"

namespace mp {

template <typename LeafLayer, typename BranchLayer, typename Feature>
class VolumeIPFMultiFeatureSelection : public PartitionForestMultiFeatureSelection<LeafLayer,BranchLayer,Feature>
{
	//#################### TYPEDEFS ####################
private:
	typedef typename BranchLayer::NodeProperties BranchProperties;
	typedef typename LeafLayer::NodeProperties LeafProperties;
	typedef PartitionForestSelection<LeafLayer,BranchLayer> PartitionForestSelectionT;
	typedef boost::shared_ptr<const PartitionForestSelectionT> PartitionForestSelection_CPtr;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	VolumeIPF_Ptr m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	explicit VolumeIPFMultiFeatureSelection(const VolumeIPF_Ptr& volumeIPF)
	:	PartitionForestMultiFeatureSelection<LeafLayer,BranchLayer,Feature>(volumeIPF), m_volumeIPF(volumeIPF)
	{}

	//#################### PUBLIC METHODS ####################
public:
	BranchProperties properties_of(const Feature& feature) const
	{
		//std::cout << "PartitionForestSelection_CPtr selection = this->selection(feature);" << std::endl;
		
		PartitionForestSelection_CPtr selection = this->selection(feature);
		
		
		if (selection->empty()) {
			std::cout << "SELECTION " << feature << " EMPTY" << std::endl;
		}
		
		//std::cout << "std::vector<BranchProperties> componentProperties;" << std::endl;
		std::vector<BranchProperties> componentProperties;
		//std::cout << "for {" << std::endl;
		for(typename PartitionForestSelectionT::NodeConstIterator it=selection->nodes_cbegin(), iend=selection->nodes_cend(); it!=iend; ++it)
		{
			//std::cout << "  (in for)" << std::endl;
			//std::cout << "  const PFNodeID& node = *it;" << std::endl;
			const PFNodeID& node = *it;
			if(node.layer() != 0)
			{
			//	std::cout << "  componentProperties.push_back(m_volumeIPF->branch_properties(node));" << std::endl;
				componentProperties.push_back(m_volumeIPF->branch_properties(node));
			}
			else
			{
				//std::cout << "  itk::Index<3> p = m_volumeIPF->position_of_leaf(node.index());" << std::endl;
				itk::Index<3> p = m_volumeIPF->position_of_leaf(node.index());
				//std::cout << "  std::pair<Vector3i,LeafProperties> leafProperties = std::make_pair(Vector3i(p[0], p[1], p[2]), m_volumeIPF->leaf_properties(node.index()));" << std::endl;	
				std::pair<Vector3i,LeafProperties> leafProperties = std::make_pair(Vector3i(p[0], p[1], p[2]), m_volumeIPF->leaf_properties(node.index()));
				//std::cout << "  componentProperties.push_back(BranchProperties::convert_from_leaf_properties(leafProperties));" << std::endl;
				componentProperties.push_back(BranchProperties::convert_from_leaf_properties(leafProperties));
			}
		}
		//std::cout << "}" << std::endl;
		//std::cout << "return" << std::endl;
		return BranchProperties::combine_branch_properties(componentProperties);
	}

	VolumeIPF_CPtr volume_ipf() const
	{
		return m_volumeIPF;
	}

	int voxel_count(const Feature& feature) const
	{
		int voxelCount = 0;
		PartitionForestSelection_CPtr selection = this->selection(feature);
		for(typename PartitionForestSelectionT::NodeConstIterator it=selection->nodes_cbegin(), iend=selection->nodes_cend(); it!=iend; ++it)
		{
			const PFNodeID& node = *it;
			if(node.layer() > 0)	voxelCount += m_volumeIPF->branch_properties(node).voxel_count();
			else					++voxelCount;
		}
		return voxelCount;
	}
};

}

#endif
