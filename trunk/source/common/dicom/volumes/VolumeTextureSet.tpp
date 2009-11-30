/***
 * millipede: VolumeTextureSet.tpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <itkExtractImageFilter.h>

#include <common/textures/TextureFactory.h>

namespace mp {

template <typename TPixel>
VolumeTextureSet::VolumeTextureSet(const typename itk::Image<TPixel,3>::ConstPointer& volumeImage, const itk::Image<TPixel,3>&)
{
	typedef itk::Image<TPixel,2> Image2D;
	typedef itk::Image<TPixel,3> Image3D;
	typedef itk::ExtractImageFilter<Image3D,Image2D> Extractor;

	typename Image3D::SizeType volumeSize = volumeImage->GetLargestPossibleRegion().GetSize();

	typename Image3D::RegionType region;
	typename Image3D::IndexType index;
	typename Image3D::SizeType size;

	// Construct the x-y textures.
	for(unsigned int z=0; z<volumeSize[2]; ++z)
	{
		typename Extractor::Pointer extractor = Extractor::New();
		extractor->SetInput(volumeImage);

		index[0] = 0;
		index[1] = 0;
		index[2] = z;
		region.SetIndex(index);

		size[0] = volumeSize[0];
		size[1] = volumeSize[1];
		size[2] = 0;
		region.SetSize(size);

		extractor->SetExtractionRegion(region);
		extractor->Update();

		m_xyTextures.push_back(TextureFactory::create_texture(extractor->GetOutput()));
	}

	// Construct the x-z textures.
	for(unsigned int y=0; y<volumeSize[1]; ++y)
	{
		typename Extractor::Pointer extractor = Extractor::New();
		extractor->SetInput(volumeImage);

		index[0] = 0;
		index[1] = y;
		index[2] = 0;
		region.SetIndex(index);

		size[0] = volumeSize[0];
		size[1] = 0;
		size[2] = volumeSize[2];
		region.SetSize(size);

		extractor->SetExtractionRegion(region);
		extractor->Update();

		m_xzTextures.push_back(TextureFactory::create_texture(extractor->GetOutput()));
	}

	// Construct the y-z textures.
	for(unsigned int x=0; x<volumeSize[0]; ++x)
	{
		typename Extractor::Pointer extractor = Extractor::New();
		extractor->SetInput(volumeImage);

		index[0] = x;
		index[1] = 0;
		index[2] = 0;
		region.SetIndex(index);

		size[0] = 0;
		size[1] = volumeSize[1];
		size[2] = volumeSize[2];
		region.SetSize(size);

		extractor->SetExtractionRegion(region);
		extractor->Update();

		m_yzTextures.push_back(TextureFactory::create_texture(extractor->GetOutput()));
	}
}

}
