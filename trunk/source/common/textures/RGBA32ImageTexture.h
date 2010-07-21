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

	//#################### PUBLIC METHODS ####################
public:
	boost::shared_ptr<ITKImageTexture<RGBA32Image> > clone() const;

	//#################### PRIVATE METHODS ####################
private:
	static std::vector<unsigned char> make_buffer(const RGBA32 *const pixels, const itk::Size<2>& size);
	void reload_image() const;
	void reload_partial_image(int minX, int minY, int maxX, int maxY) const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<RGBA32ImageTexture> RGBA32ImageTexture_Ptr;

}

#endif
