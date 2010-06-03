/***
 * millipede: RGB24ImageTexture.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "RGB24ImageTexture.h"

namespace mp {

//#################### CONSTRUCTORS ####################
RGB24ImageTexture::RGB24ImageTexture(const ImagePointer& image, const boost::optional<RGB24>& colourKey, bool clamp)
:	ITKImageTexture<RGB24Image>(image, clamp)
{
	reload();
}

//#################### PRIVATE METHODS ####################
void RGB24ImageTexture::reload_image() const
{
	// NYI
	throw 23;
}

}
