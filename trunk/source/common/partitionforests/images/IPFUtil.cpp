/***
 * millipede: IPFUtil.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "IPFUtil.h"

#include <itkShapedNeighborhoodIterator.h>
#include <itkZeroFluxNeumannBoundaryCondition.h>

#include <common/util/ITKImageUtil.h>

namespace mp {

namespace IPFUtil {

itk::Image<unsigned char,2>::Pointer make_mosaic_image(const boost::shared_ptr<const PartitionForest<CTImageLeafLayer,CTImageBranchLayer> >& ipf, int layerIndex,
													   int width, int height)
{
	typedef itk::Image<unsigned char,2> Image;
	typedef PartitionForest<CTImageLeafLayer,CTImageBranchLayer> IPF;

	Image::Pointer image = ITKImageUtil::make_image<unsigned char>(width, height);

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

itk::Image<unsigned char,2>::Pointer make_mosaic_image_with_boundaries(const boost::shared_ptr<const PartitionForest<CTImageLeafLayer,CTImageBranchLayer> >& ipf,
																	   int layerIndex, int width, int height)
{
	typedef itk::Image<PFNodeID,2> AncestorImage;
	typedef itk::Image<unsigned char,2> MosaicImage;
	typedef PartitionForest<CTImageLeafLayer,CTImageBranchLayer> IPF;

	// Create an image of the ancestors of the pixels in the specified layer.
	AncestorImage::Pointer ancestorImage = ITKImageUtil::make_image<PFNodeID>(width, height);

	AncestorImage::IndexType ancestorIndex;
	int n = 0;
	for(ancestorIndex[1]=0; ancestorIndex[1]<height; ++ancestorIndex[1])
		for(ancestorIndex[0]=0; ancestorIndex[0]<width; ++ancestorIndex[0])
		{
			ancestorImage->SetPixel(ancestorIndex, ipf->ancestor_of(PFNodeID(0, n), layerIndex));
			++n;
		}

	// Set up an iterator to traverse the ancestor image, whilst allowing us to access the neighbours of each pixel.
	typedef itk::ConstShapedNeighborhoodIterator<AncestorImage> ConstShapedNeighbourhoodIteratorType;
	AncestorImage::SizeType radius;
	radius.Fill(1);
	ConstShapedNeighbourhoodIteratorType it(radius, ancestorImage, ancestorImage->GetLargestPossibleRegion());
	std::vector<AncestorImage::OffsetType> offsets(4);
	offsets[0][0] = 0;	offsets[0][1] = -1;
	offsets[1][0] = -1;	offsets[1][1] = 0;
	offsets[2][0] = 1;	offsets[2][1] = 0;
	offsets[3][0] = 0;	offsets[3][1] = 1;
	for(size_t k=0, size=offsets.size(); k<size; ++k)
	{
		it.ActivateOffset(offsets[k]);
	}

	// Set up a boundary condition that makes pixels beyond the boundary equal to those on it. This is the
	// right boundary condition here, because the idea is to mark pixels as boundaries when they have an
	// adjacent neighbour with a different ancestor. We don't want there to be spurious boundaries on the
	// borders of the image, so we need to make sure that the pixels beyond the image have the same ancestors
	// as their respective neighbours within it.
	itk::ZeroFluxNeumannBoundaryCondition<AncestorImage> condition;
	it.OverrideBoundaryCondition(&condition);

	// Create the mosaic image by traversing the ancestor image. We mark boundaries where appropriate, and
	// obtain the non-boundary mosaic values from the properties of the ancestor nodes.
	MosaicImage::Pointer mosaicImage = ITKImageUtil::make_image<unsigned char>(width, height);

	for(it.GoToBegin(); !it.IsAtEnd(); ++it)
	{
		bool regionBoundary = false;
		for(ConstShapedNeighbourhoodIteratorType::ConstIterator jt=it.Begin(), jend=it.End(); jt!=jend; ++jt)
		{
			if(jt.Get() != it.GetCenterPixel())
			{
				regionBoundary = true;
				break;
			}
		}

		unsigned char mosaicValue;
		if(regionBoundary)		mosaicValue = std::numeric_limits<unsigned char>::max();
		else if(layerIndex > 0)	mosaicValue = static_cast<unsigned char>(ipf->branch_properties(it.GetCenterPixel()).mean_grey_value());
		else					mosaicValue = ipf->leaf_properties(it.GetCenterPixel().index()).grey_value();
		mosaicImage->SetPixel(it.GetIndex(), mosaicValue);
	}

	return mosaicImage;
}

}

}
