/***
 * millipede: RGB24ImageTexture.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_RGB24IMAGETEXTURE
#define H_MILLIPEDE_RGB24IMAGETEXTURE

#include <boost/optional.hpp>

#include <itkImage.h>
#include <itkRGBPixel.h>

#include "Texture.h"

namespace mp {

class RGB24ImageTexture : public Texture
{
	//#################### FRIENDS ####################
	friend class TextureFactory;

	//#################### TYPEDEFS ####################
private:
	typedef itk::RGBPixel<unsigned char> RGB24;
	typedef itk::Image<RGB24> Image;
	typedef Image::Pointer ImagePointer;

	//#################### CONSTRUCTORS ####################
private:
	RGB24ImageTexture(const ImagePointer& image, const boost::optional<RGB24>& colourKey, bool clamp);

	//#################### PROTECTED METHODS ####################
protected:
	void reload_image() const;
};

}

#endif
