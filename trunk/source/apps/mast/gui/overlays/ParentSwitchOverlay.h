/***
 * millipede: ParentSwitchOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARENTSWITCHOVERLAY
#define H_MILLIPEDE_PARENTSWITCHOVERLAY

#include <common/partitionforests/images/VolumeIPF.h>
#include "IPFOverlayTools.h"
#include "PartitionOverlay.h"

namespace mp {

class ParentSwitchOverlay : public PartitionOverlay
{
	//#################### CONSTRUCTORS ####################
public:
	template <typename LeafLayer, typename BranchLayer>
	ParentSwitchOverlay(const std::set<PFNodeID>& potentialNewParents, const boost::shared_ptr<const VolumeIPF<LeafLayer,BranchLayer> >& volumeIPF,
						const SliceLocation& sliceLocation, SliceOrientation sliceOrientation)
	{
		itk::Index<3> sliceBegin, sliceEnd;
		int width, height;
		IPFOverlayTools::calculate_slice_parameters(volumeIPF->volume_size(), sliceLocation, sliceOrientation, sliceBegin, sliceEnd, width, height);

		RGBA32Image::Pointer image = ITKImageUtil::make_image<RGBA32>(width, height);

		RGBA32 colour = ITKImageUtil::make_rgba32(0,255,0,50);
		for(std::set<PFNodeID>::const_iterator it=potentialNewParents.begin(), iend=potentialNewParents.end(); it!=iend; ++it)
		{
			// Only draw the potential new parents if we're viewing the layer they're in.
			// Note that they're all in the same layer, so we can break if the first one
			// isn't in the layer we're viewing.
			if(it->layer() != sliceLocation.layer) break;

			IPFOverlayTools::draw_node(volumeIPF, *it, image, sliceBegin, sliceEnd, sliceOrientation, colour, boost::none, boost::none);
		}

		set_texture(TextureFactory::create_texture(image));
	}

	//#################### PUBLIC METHODS ####################
public:
	bool on_dicom_canvas() const
	{
		return false;
	}
};

}

#endif
