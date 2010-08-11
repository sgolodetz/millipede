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
