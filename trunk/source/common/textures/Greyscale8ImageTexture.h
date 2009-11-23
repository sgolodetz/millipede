/***
 * millipede: Greyscale8ImageTexture.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_GREYSCALE8IMAGETEXTURE
#define H_MILLIPEDE_GREYSCALE8IMAGETEXTURE

#include <itkImage.h>

#include "Texture.h"

namespace mp {

class Greyscale8ImageTexture : public Texture
{
	//#################### FRIENDS ####################
	friend class TextureFactory;

	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<unsigned char,2> Image;
	typedef Image::Pointer ImagePointer;

	//#################### PRIVATE VARIABLES ####################
private:
	ImagePointer m_image;

	//#################### CONSTRUCTORS ####################
protected:
	Greyscale8ImageTexture(const ImagePointer& image, bool clamp);

	//#################### PROTECTED METHODS ####################
protected:
	void reload_image() const;
};

}

#endif
