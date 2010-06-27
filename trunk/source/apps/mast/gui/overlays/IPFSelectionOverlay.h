/***
 * millipede: IPFSelectionOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFSELECTIONOVERLAY
#define H_MILLIPEDE_IPFSELECTIONOVERLAY

#include <common/partitionforests/images/VolumeIPFSelection.h>
#include "IPFOverlayTools.h"
#include "PartitionOverlay.h"

namespace mp {

class IPFSelectionOverlay : public PartitionOverlay
{
	//#################### CONSTRUCTORS ####################
public:
	template <typename LeafLayer, typename BranchLayer>
	IPFSelectionOverlay(const boost::shared_ptr<const VolumeIPFSelection<LeafLayer,BranchLayer> >& selection, const SliceLocation& sliceLocation,
						SliceOrientation sliceOrientation)
	{
		boost::shared_ptr<const VolumeIPF<LeafLayer,BranchLayer> > volumeIPF = selection->volume_ipf();
		itk::Index<3> sliceBegin, sliceEnd;
		int width, height;
		IPFOverlayTools::calculate_slice_parameters(volumeIPF->volume_size(), sliceLocation, sliceOrientation, sliceBegin, sliceEnd, width, height);

		RGBA32Image::Pointer image = ITKImageUtil::make_image<RGBA32>(width, height);

		if(sliceLocation.layer <= volumeIPF->highest_layer())
		{
			// Fill colour.
			RGBA32 fc = ITKImageUtil::make_rgba32(255,0,0,50);

			typedef typename VolumeIPFSelection<LeafLayer,BranchLayer>::NodeConstIterator Iter;
			for(Iter it=selection->nodes_cbegin(), iend=selection->nodes_cend(); it!=iend; ++it)
			{
				IPFOverlayTools::draw_node(volumeIPF, *it, image, sliceBegin, sliceEnd, sliceOrientation, fc, false);
			}
		}

		set_texture(TextureFactory::create_texture(image));
	}
};

}

#endif
