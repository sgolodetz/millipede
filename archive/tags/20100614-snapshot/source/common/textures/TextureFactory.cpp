/***
 * millipede: TextureFactory.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "TextureFactory.h"

#include <common/exceptions/Exception.h>
#include "Greyscale8ImageTexture.h"
#include "RGB24ImageTexture.h"
#include "RGBA32ImageTexture.h"

namespace mp {

//#################### PUBLIC METHODS ####################
Texture_Ptr TextureFactory::create_texture(const Greyscale8Image::Pointer& image, bool clamp)
{
	check_dimensions(image->GetLargestPossibleRegion().GetSize());
	return Texture_Ptr(new Greyscale8ImageTexture(image, clamp));
}

Texture_Ptr TextureFactory::create_texture(const RGB24Image::Pointer& image, const boost::optional<RGB24>& colourKey, bool clamp)
{
	check_dimensions(image->GetLargestPossibleRegion().GetSize());
	return Texture_Ptr(new RGB24ImageTexture(image, colourKey, clamp));
}

Texture_Ptr TextureFactory::create_texture(const RGBA32Image::Pointer& image, bool clamp)
{
	check_dimensions(image->GetLargestPossibleRegion().GetSize());
	return Texture_Ptr(new RGBA32ImageTexture(image, clamp));
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
