/***
 * millipede: RGBA32ImageTexture.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_RGBA32IMAGETEXTURE
#define H_MILLIPEDE_RGBA32IMAGETEXTURE

#include <itkRGBAPixel.h>

#include "ITKImageTexture.h"

namespace mp {

//#################### TYPEDEFS ####################
typedef itk::RGBAPixel<unsigned char> RGBA32;
typedef itk::Image<RGBA32,2> RGBA32Image;

class RGBA32ImageTexture : public ITKImageTexture<RGBA32Image>
{
	//#################### FRIENDS ####################
	friend class TextureFactory;

	//#################### CONSTRUCTORS ####################
public:
	explicit RGBA32ImageTexture(const ImagePointer& image, bool clamp);

	//#################### PRIVATE METHODS ####################
private:
	void reload_image() const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<RGBA32ImageTexture> RGBA32ImageTexture_Ptr;

}

#endif
