/***
 * millipede: RGB24ImageTexture.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_RGB24IMAGETEXTURE
#define H_MILLIPEDE_RGB24IMAGETEXTURE

#include <boost/optional.hpp>

#include <itkRGBPixel.h>

#include "ITKImageTexture.h"

namespace mp {

//#################### TYPEDEFS ####################
typedef itk::RGBPixel<unsigned char> RGB24;
typedef itk::Image<RGB24> RGB24Image;

class RGB24ImageTexture : public ITKImageTexture<RGB24Image>
{
	//#################### FRIENDS ####################
	friend class TextureFactory;

	//#################### CONSTRUCTORS ####################
private:
	RGB24ImageTexture(const ImagePointer& image, const boost::optional<RGB24>& colourKey, bool clamp);

	//#################### PRIVATE METHODS ####################
private:
	void reload_image() const;
};

}

#endif
