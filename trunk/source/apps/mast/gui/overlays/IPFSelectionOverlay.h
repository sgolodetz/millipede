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
	explicit IPFSelectionOverlay(const boost::shared_ptr<const VolumeIPFSelection<LeafLayer,BranchLayer> >& selection,
								 const SliceLocation& sliceLocation, SliceOrientation sliceOrientation)
	{
		boost::shared_ptr<const VolumeIPF<LeafLayer,BranchLayer> > volumeIPF = selection->volume_ipf();
		itk::Index<3> sliceBegin, sliceEnd;
		int width, height;
		IPFOverlayTools::calculate_slice_parameters(volumeIPF->volume_size(), sliceLocation, sliceOrientation, sliceBegin, sliceEnd, width, height);

		RGBA32Image::Pointer image = ITKImageUtil::make_image<RGBA32>(width, height);

		if(sliceLocation.layer <= volumeIPF->highest_layer())
		{
			RGBA32 fc, bc;	// fill and boundary colours
			fc[0] = 255, fc[1] = 0, fc[2] = 0, fc[3] = 50;
			bc[0] = 255, bc[1] = 0, bc[2] = 0, bc[3] = 255;

			typedef typename VolumeIPFSelection<LeafLayer,BranchLayer>::ViewNodeConstIterator Iter;
			for(Iter it=selection->view_at_layer_cbegin(sliceLocation.layer), iend=selection->view_at_layer_cend(sliceLocation.layer); it!=iend; ++it)
			{
				IPFOverlayTools::draw_node(volumeIPF, *it, image, sliceBegin, sliceEnd, sliceOrientation, fc, false);
				IPFOverlayTools::draw_node(volumeIPF, *it, image, sliceBegin, sliceEnd, sliceOrientation, bc, true);
			}
		}

		set_texture(TextureFactory::create_texture(image));
	}

	//#################### PUBLIC METHODS ####################
public:
	std::string name() const
	{
		return "IPFSelection";
	}
};

}

#endif
