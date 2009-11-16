/***
 * millipede: Volume.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Volume.h"

#include <itkIntensityWindowingImageFilter.h>

#include <common/dicom/util/WindowSettings.h>

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
	typedef itk::IntensityWindowingImageFilter<BaseImage,WindowedImage> Windower;

	Windower::Pointer windower = Windower::New();
	windower->SetInput(m_baseImage);
	windower->SetWindowLevel(windowSettings.width(), windowSettings.centre());
	windower->SetOutputMinimum(0);
	windower->SetOutputMaximum(255);
	windower->Update();

	return windower->GetOutput();
}

}
