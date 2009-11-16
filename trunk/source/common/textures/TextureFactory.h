/***
 * millipede: TextureFactory.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TEXTUREFACTORY
#define H_MILLIPEDE_TEXTUREFACTORY

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <itkImage.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class Texture> Texture_Ptr;

class TextureFactory
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<unsigned char,2> Greyscale8Image;
	typedef itk::Image<unsigned char,2>::ConstPointer Greyscale8ImageCPointer;

	//#################### PUBLIC METHODS ####################
public:
	static Texture_Ptr create_texture(const Greyscale8ImageCPointer& image, bool clamp = true);

	//#################### PRIVATE METHODS ####################
private:
	static void check_dimensions(int width, int height);
};

}

#endif
