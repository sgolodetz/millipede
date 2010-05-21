/***
 * millipede: IPFUtil.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "IPFUtil.h"

namespace mp {

namespace IPFUtil {

itk::Image<unsigned char,2>::Pointer make_mosaic_image(const boost::shared_ptr<const PartitionForest<CTImageLeafLayer,CTImageBranchLayer> >& ipf, int layerIndex,
													   int width, int height)
{
	typedef itk::Image<unsigned char,2> Image;
	typedef PartitionForest<CTImageLeafLayer,CTImageBranchLayer> IPF;

	Image::IndexType start;
	start.Fill(0);
	Image::SizeType size;
	size[0] = width;
	size[1] = height;
	Image::RegionType region;
	region.SetIndex(start);
	region.SetSize(size);
	Image::Pointer image = Image::New();
	image->SetRegions(region);
	image->Allocate();

	Image::IndexType index;
	int n = 0;
	for(index[1]=0; index[1]<height; ++index[1])
		for(index[0]=0; index[0]<width; ++index[0])
		{
			unsigned char mosaicValue;
			if(layerIndex > 0)
			{
				PFNodeID ancestor = ipf->ancestor_of(PFNodeID(0, n), layerIndex);
				mosaicValue = static_cast<unsigned char>(ipf->branch_properties(ancestor).mean_grey_value());
			}
			else mosaicValue = ipf->leaf_properties(n).grey_value();

			image->SetPixel(index, mosaicValue);
			++n;
		}

	return image;
}

}

}
