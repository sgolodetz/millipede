/***
 * millipede: Volume.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Volume.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Volume::Volume(const ImagePointer& baseImage)
:	m_baseImage(baseImage)
{}

//#################### PUBLIC METHODS ####################
Volume::ImageCPointer Volume::base_image() const
{
	return ImageCPointer(m_baseImage);
}

Volume::ImageCPointer Volume::windowed_image(const WindowSettings& windowSettings) const
{
	// NYI
	throw 23;
}

}
