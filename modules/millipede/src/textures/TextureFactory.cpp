/***
 * millipede: TextureFactory.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "textures/TextureFactory.h"

#include "exceptions/Exception.h"

namespace mp {

//#################### PUBLIC METHODS ####################
Greyscale8ImageTexture_Ptr TextureFactory::create_texture(const Greyscale8Image::Pointer& image, bool clamp)
{
	check_dimensions(image->GetLargestPossibleRegion().GetSize());
	return Greyscale8ImageTexture_Ptr(new Greyscale8ImageTexture(image, clamp));
}

RGB24ImageTexture_Ptr TextureFactory::create_texture(const RGB24Image::Pointer& image, const boost::optional<RGB24>& colourKey, bool clamp)
{
	check_dimensions(image->GetLargestPossibleRegion().GetSize());
	return RGB24ImageTexture_Ptr(new RGB24ImageTexture(image, colourKey, clamp));
}

RGBA32ImageTexture_Ptr TextureFactory::create_texture(const RGBA32Image::Pointer& image, bool clamp)
{
	check_dimensions(image->GetLargestPossibleRegion().GetSize());
	return RGBA32ImageTexture_Ptr(new RGBA32ImageTexture(image, clamp));
}

//#################### PRIVATE METHODS ####################
void TextureFactory::check_dimensions(int width, int height)
{
	if(width < 1 || width > 1024 || height < 1 || height > 1024) throw Exception("Image dimensions out of range");
}

void TextureFactory::check_dimensions(const itk::Size<2>& size)
{
	check_dimensions(size[0], size[1]);
}

}
