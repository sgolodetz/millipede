/***
 * millipede: Greyscale8ImageTexture.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Greyscale8ImageTexture.h"

#include <gl/glu.h>

namespace mp {

//#################### CONSTRUCTORS ####################
Greyscale8ImageTexture::Greyscale8ImageTexture(const ImageCPointer& image, bool clamp)
:	Texture(clamp), m_image(image)
{}

//#################### PROTECTED METHODS ####################
void Greyscale8ImageTexture::reload_image() const
{
	Image::SizeType size = m_image->GetLargestPossibleRegion().GetSize();
	gluBuild2DMipmaps(GL_TEXTURE_2D, 1, size[0], size[1], GL_LUMINANCE, GL_UNSIGNED_BYTE, m_image->GetBufferPointer());
}

}
