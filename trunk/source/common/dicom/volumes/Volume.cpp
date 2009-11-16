/***
 * millipede: Volume.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Volume.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Volume::Volume(const BaseImagePointer& baseImage)
:	m_baseImage(baseImage)
{}

//#################### PUBLIC METHODS ####################
Volume::BaseImageCPointer Volume::base_image() const
{
	return BaseImageCPointer(m_baseImage);
}

Volume::Size Volume::size() const
{
	return m_baseImage->GetLargestPossibleRegion().GetSize();
}

Volume::WindowedImageCPointer Volume::windowed_image(const WindowSettings& windowSettings) const
{
	// NYI
	throw 23;
}

}
