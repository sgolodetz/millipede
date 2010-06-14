/***
 * millipede: VolumeIPFSelection.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMEIPFSELECTION
#define H_MILLIPEDE_VOLUMEIPFSELECTION

#include <common/partitionforests/base/PartitionForestSelection.h>
#include "VolumeIPF.h"

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class VolumeIPFSelection : public PartitionForestSelection<LeafLayer,BranchLayer>
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
	explicit VolumeIPFSelection(const VolumeIPF_Ptr& volumeIPF)
	:	PartitionForestSelection<LeafLayer,BranchLayer>(volumeIPF), m_volumeIPF(volumeIPF)
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
