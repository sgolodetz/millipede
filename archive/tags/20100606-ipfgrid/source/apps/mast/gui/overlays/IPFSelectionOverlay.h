/***
 * millipede: IPFSelectionOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFSELECTIONOVERLAY
#define H_MILLIPEDE_IPFSELECTIONOVERLAY

#include <common/partitionforests/images/IPFSelectionGrid.h>
#include <common/slices/SliceLocation.h>
#include <common/slices/SliceOrientation.h>
#include <common/textures/RGBA32ImageTexture.h>
#include "PartitionOverlay.h"

namespace mp {

class IPFSelectionOverlay : public PartitionOverlay
{
	//#################### CONSTRUCTORS ####################
public:
	template <typename IPFSelection>
	explicit IPFSelectionOverlay(boost::shared_ptr<const IPFSelectionGrid<IPFSelection> > selectionGrid, const SliceLocation& sliceLocation,
								 SliceOrientation sliceOrientation)
	{
		// TEMPORARY (test code)
		RGBA32Image::Pointer colouredImage = ITKImageUtil::make_image<RGBA32>(512, 512);
		for(int i=0; i<512; ++i)
		{
			for(int j=0; j<512; ++j)
			{
				itk::Index<2> index = {{i,j}};
				RGBA32 p;
				p[0] = 255, p[1] = 0, p[2] = 0, p[3] = 0;
				colouredImage->SetPixel(index, p);
			}
		}
		set_texture(TextureFactory::create_texture(colouredImage));
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
