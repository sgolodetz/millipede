/***
 * millipede: TextureFactory.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TEXTUREFACTORY
#define H_MILLIPEDE_TEXTUREFACTORY

#include <boost/optional.hpp>

#include "Greyscale8ImageTexture.h"
#include "RGB24ImageTexture.h"
#include "RGBA32ImageTexture.h"

namespace mp {

class TextureFactory
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<unsigned char,2> Greyscale8Image;

	typedef itk::RGBPixel<unsigned char> RGB24;
	typedef itk::Image<RGB24,2> RGB24Image;

	typedef itk::RGBAPixel<unsigned char> RGBA32;
	typedef itk::Image<RGBA32,2> RGBA32Image;

	//#################### PUBLIC METHODS ####################
public:
	static Greyscale8ImageTexture_Ptr create_texture(const Greyscale8Image::Pointer& image, bool clamp = true);
	static RGB24ImageTexture_Ptr create_texture(const RGB24Image::Pointer& image, const boost::optional<RGB24>& colourKey = boost::none, bool clamp = true);
	static RGBA32ImageTexture_Ptr create_texture(const RGBA32Image::Pointer& image, bool clamp = true);

	//#################### PRIVATE METHODS ####################
private:
	static void check_dimensions(int width, int height);
	static void check_dimensions(const itk::Size<2>& size);
};

}

#endif
