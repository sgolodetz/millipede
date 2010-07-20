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
typedef itk::Image<RGB24,2> RGB24Image;

class RGB24ImageTexture : public ITKImageTexture<RGB24Image>
{
	//#################### FRIENDS ####################
	friend class TextureFactory;

	//#################### PRIVATE VARIABLES ####################
private:
	boost::optional<RGB24> m_colourKey;

	//#################### CONSTRUCTORS ####################
private:
	RGB24ImageTexture(const ImagePointer& image, const boost::optional<RGB24>& colourKey, bool clamp);

	//#################### PRIVATE METHODS ####################
private:
	void reload_image() const;
	void reload_image_with_colour_key(const RGB24 *const pixels, const itk::Size<2>& size) const;
	void reload_image_without_colour_key(const RGB24 *const pixels, const itk::Size<2>& size) const;
	void reload_partial_image(int minX, int minY, int maxX, int maxY) const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<RGB24ImageTexture> RGB24ImageTexture_Ptr;

}

#endif
