/***
 * millipede: TextureFactory.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TEXTUREFACTORY
#define H_MILLIPEDE_TEXTUREFACTORY

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <itkImage.h>
#include <itkRGBPixel.h>
#include <itkRGBAPixel.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class Texture> Texture_Ptr;

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
	static Texture_Ptr create_texture(const Greyscale8Image::Pointer& image, bool clamp = true);
	static Texture_Ptr create_texture(const RGB24Image::Pointer& image, const boost::optional<RGB24>& colourKey = boost::none, bool clamp = true);
	static Texture_Ptr create_texture(const RGBA32Image::Pointer& image, bool clamp = true);

	//#################### PRIVATE METHODS ####################
private:
	static void check_dimensions(int width, int height);
	static void check_dimensions(const itk::Size<2>& size);
};

}

#endif
