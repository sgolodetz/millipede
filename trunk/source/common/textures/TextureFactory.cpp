/***
 * millipede: TextureFactory.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "TextureFactory.h"

#include <common/exceptions/Exception.h>
#include "Greyscale8ImageTexture.h"

namespace mp {

//#################### PUBLIC METHODS ####################
Texture_Ptr TextureFactory::create_texture(const Greyscale8ImagePointer& image, bool clamp)
{
	Greyscale8Image::SizeType size = image->GetLargestPossibleRegion().GetSize();
	check_dimensions(size[0], size[1]);
	return Texture_Ptr(new Greyscale8ImageTexture(image, clamp));
}

//#################### PRIVATE METHODS ####################
void TextureFactory::check_dimensions(int width, int height)
{
	if(width < 1 || width > 1024 || height < 1 || height > 1024) throw Exception("Image dimensions out of range");
}

}
