/***
 * millipede: VolumeTextureSet.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeTextureSet.h"

#include <itkExtractImageFilter.h>

#include <common/textures/TextureFactory.h>
#include "Volume.h"

namespace mp {

//#################### CONSTRUCTORS ####################
VolumeTextureSet::VolumeTextureSet(const Volume_CPtr& volume, const WindowSettings& windowSettings)
{
	typedef itk::Image<unsigned char,2> Image2D;
	typedef itk::ExtractImageFilter<Volume::WindowedImage,Image2D> Extractor;

	Volume::WindowedImageCPointer windowedImage = volume->windowed_image(windowSettings);
	Volume::WindowedImage::SizeType volumeSize = windowedImage->GetLargestPossibleRegion().GetSize();

	Volume::WindowedImage::RegionType region;
	Volume::WindowedImage::IndexType index;
	Volume::WindowedImage::SizeType size;

	// Construct the x-y textures.
	for(unsigned int z=0; z<volumeSize[2]; ++z)
	{
		Extractor::Pointer extractor = Extractor::New();
		extractor->SetInput(windowedImage);

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
		Extractor::Pointer extractor = Extractor::New();
		extractor->SetInput(windowedImage);

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
		Extractor::Pointer extractor = Extractor::New();
		extractor->SetInput(windowedImage);

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

//#################### PUBLIC METHODS ####################
Texture_CPtr VolumeTextureSet::xy_texture(int n) const
{
	assert(0 <= n && n < static_cast<int>(m_xyTextures.size()));
	return m_xyTextures[n];
}

Texture_CPtr VolumeTextureSet::xz_texture(int n) const
{
	assert(0 <= n && n < static_cast<int>(m_xzTextures.size()));
	return m_xzTextures[n];
}

Texture_CPtr VolumeTextureSet::yz_texture(int n) const
{
	assert(0 <= n && n < static_cast<int>(m_yzTextures.size()));
	return m_yzTextures[n];
}

}
