/***
 * millipede: DICOMVolume.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "DICOMVolume.h"

#include <itkIntensityWindowingImageFilter.h>

#include <common/dicom/util/WindowSettings.h>

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMVolume::DICOMVolume(const BaseImagePointer& baseImage)
:	m_baseImage(baseImage)
{}

//#################### PUBLIC METHODS ####################
DICOMVolume::BaseImagePointer DICOMVolume::base_image() const
{
	return m_baseImage;
}

itk::Size<3> DICOMVolume::size() const
{
	return m_baseImage->GetLargestPossibleRegion().GetSize();
}

itk::Vector<double,3> DICOMVolume::spacing() const
{
	return m_baseImage->GetSpacing();
}

DICOMVolume::WindowedImagePointer DICOMVolume::windowed_image(const WindowSettings& windowSettings) const
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
