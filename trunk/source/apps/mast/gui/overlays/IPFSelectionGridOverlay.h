/***
 * millipede: IPFSelectionGridOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFSELECTIONGRIDOVERLAY
#define H_MILLIPEDE_IPFSELECTIONGRIDOVERLAY

#include <common/partitionforests/images/IPFSelectionGrid.h>
#include <common/textures/RGB24ImageTexture.h>
#include "PartitionOverlay.h"

namespace mp {

class IPFSelectionGridOverlay : public PartitionOverlay
{
	//#################### CONSTRUCTORS ####################
public:
	template <typename IPFSelection>
	explicit IPFSelectionGridOverlay(boost::shared_ptr<const IPFSelectionGrid<IPFSelection> > selectionGrid)
	{
		// TEMPORARY (test code)
		RGB24Image::Pointer colouredImage = ITKImageUtil::make_image<RGB24>(512, 512);
		for(int i=0; i<512; ++i)
		{
			for(int j=0; j<512; ++j)
			{
				itk::Index<2> index = {{i,j}};
				RGB24 p;
				p[0] = /*i == j ? 255 :*/ 0, p[1] = 0, p[2] = 0;
				colouredImage->SetPixel(index, p);
			}
		}
		RGB24 colourKey;
		colourKey.Fill(0);

		set_texture(TextureFactory::create_texture(colouredImage, colourKey));
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
