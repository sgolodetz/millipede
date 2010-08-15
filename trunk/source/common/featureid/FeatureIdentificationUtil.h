/***
 * millipede: FeatureIdentificationUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FEATUREIDENTIFICATIONUTIL
#define H_MILLIPEDE_FEATUREIDENTIFICATIONUTIL

#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>

namespace mp {

namespace FeatureIdentificationUtil {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

//#################### TYPEDEFS ####################
typedef DICOMImageBranchLayer BranchLayer;
typedef AbdominalFeature::Enum Feature;
typedef DICOMImageLeafLayer LeafLayer;
typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;
typedef VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> VolumeIPFMultiFeatureSelectionT;
typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;

typedef VolumeIPFT::BranchNodeConstIterator BranchNodeConstIterator;
typedef VolumeIPFT::BranchProperties BranchProperties;

//#################### TEMPLATE FUNCTIONS ####################
template <typename Pred>
std::list<PFNodeID> filter_branch_nodes(const VolumeIPF_CPtr& volumeIPF, Pred pred)
{
	std::list<PFNodeID> result;
	for(int layer=1, highestLayer=volumeIPF->highest_layer(); layer<=highestLayer; ++layer)
	{
		for(BranchNodeConstIterator it=volumeIPF->branch_nodes_cbegin(layer), iend=volumeIPF->branch_nodes_cend(layer); it!=iend; ++it)
		{
			if(pred(it->properties()))
			{
				result.push_back(PFNodeID(layer, it.index()));
			}
		}
	}
	return result;
}

}

}

#endif
