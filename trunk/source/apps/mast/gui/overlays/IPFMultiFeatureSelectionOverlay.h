/***
 * millipede: IPFMultiFeatureSelectionOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFMULTIFEATURESELECTIONOVERLAY
#define H_MILLIPEDE_IPFMULTIFEATURESELECTIONOVERLAY

#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>
#include "IPFOverlayTools.h"
#include "PartitionOverlay.h"

namespace mp {

class IPFMultiFeatureSelectionOverlay : public PartitionOverlay
{
	//#################### CONSTRUCTORS ####################
public:
	template <typename LeafLayer, typename BranchLayer, typename FeatureID>
	IPFMultiFeatureSelectionOverlay(const boost::shared_ptr<const VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,FeatureID> >& multiFeatureSelection,
									const SliceLocation& sliceLocation, SliceOrientation sliceOrientation)
	{
		// TEMPORARY
		RGBA32Image::Pointer image = ITKImageUtil::make_image<RGBA32>(512, 512);
		set_texture(TextureFactory::create_texture(image));
	}
};

}

#endif
