/***
 * millipede: Greyscale8ImageTexture.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Greyscale8ImageTexture.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Greyscale8ImageTexture::Greyscale8ImageTexture(const ImagePointer& image, bool clamp)
:	ITKImageTexture(image, clamp)
{
	reload();
}

//#################### PRIVATE METHODS ####################
void Greyscale8ImageTexture::reload_image() const
{
	ImagePointer input = input_image();
	itk::Size<2> size = input->GetLargestPossibleRegion().GetSize();
	glTexImage2D(GL_TEXTURE_2D, 0, 1, size[0], size[1], 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, input_image()->GetBufferPointer());
}

}
