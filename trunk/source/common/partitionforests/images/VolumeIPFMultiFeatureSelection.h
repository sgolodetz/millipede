/***
 * millipede: VolumeIPFMultiFeatureSelection.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMEIPFMULTIFEATURESELECTION
#define H_MILLIPEDE_VOLUMEIPFMULTIFEATURESELECTION

#include <common/partitionforests/base/PartitionForestMultiFeatureSelection.h>
#include "VolumeIPF.h"

namespace mp {

template <typename LeafLayer, typename BranchLayer, typename FeatureID>
class VolumeIPFMultiFeatureSelection : public PartitionForestMultiFeatureSelection<LeafLayer,BranchLayer,FeatureID>
{
	//#################### TYPEDEFS ####################
private:
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	VolumeIPF_Ptr m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	explicit VolumeIPFMultiFeatureSelection(const VolumeIPF_Ptr& volumeIPF)
	:	PartitionForestMultiFeatureSelection<LeafLayer,BranchLayer,FeatureID>(volumeIPF), m_volumeIPF(volumeIPF)
	{}

	//#################### PUBLIC METHODS ####################
public:
	VolumeIPF_CPtr volume_ipf() const
	{
		return m_volumeIPF;
	}
};

}

#endif
