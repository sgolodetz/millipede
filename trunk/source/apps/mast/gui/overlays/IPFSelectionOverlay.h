/***
 * millipede: IPFSelectionOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFSELECTIONOVERLAY
#define H_MILLIPEDE_IPFSELECTIONOVERLAY

#include <iostream>

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
		
		std::cout << "calculate_slice_parameters" << std::endl;
		IPFOverlayTools::calculate_slice_parameters(volumeIPF->volume_size(), sliceLocation, sliceOrientation, sliceBegin, sliceEnd, width, height);

		std::cout << "make_image" << std::endl;
		RGBA32Image::Pointer image = ITKImageUtil::make_image<RGBA32>(width, height);

		std::cout << "if(sliceLocation.layer <= volumeIPF->highest_layer())" << std::endl;
		if(sliceLocation.layer <= volumeIPF->highest_layer())
		{
			std::cout << "make_rgba32" << std::endl;
			RGBA32 fillColour = ITKImageUtil::make_rgba32(255,0,0,50);
			RGBA32 boundaryColour = ITKImageUtil::make_rgba32(255,0,0,255);
			typedef typename VolumeIPFSelection<LeafLayer,BranchLayer>::ViewNodeConstIterator Iter;
			std::cout << "for(Iter it=selection->view_at_layer_cbegin(sliceLocation.layer)" << std::endl;
			for(Iter it=selection->view_at_layer_cbegin(sliceLocation.layer), iend=selection->view_at_layer_cend(sliceLocation.layer); it!=iend; ++it)
			{
				std::cout << "if(it->layer() != 0)" << std::endl;
				if(it->layer() != 0)
				{
					std::cout << "if" << std::endl;
					IPFOverlayTools::draw_node(volumeIPF, *it, image, sliceBegin, sliceEnd, sliceOrientation, fillColour, boundaryColour, boost::none);
				}
				else
				{
					std::cout << "else" << std::endl;
					IPFOverlayTools::draw_node(volumeIPF, *it, image, sliceBegin, sliceEnd, sliceOrientation, fillColour, boost::none, boost::none);
				}
				std::cout << "endif" << std::endl;
			}
		}

		std::cout << "set_texture(TextureFactory::create_texture(image));" << std::endl;
		set_texture(TextureFactory::create_texture(image));
	}
};

}

#endif
