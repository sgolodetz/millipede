/***
 * millipede: DICOMVolume.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "DICOMVolume.h"

#include <itkIntensityWindowingImageFilter.h>

#include <common/dicom/util/WindowSettings.h>

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMVolume::DICOMVolume(const BaseImagePointer& baseImage, Modality modality)
:	m_baseImage(baseImage), m_modality(modality)
{}

//#################### PUBLIC METHODS ####################
DICOMVolume::BaseImagePointer DICOMVolume::base_image() const
{
	return m_baseImage;
}

DICOMVolume::Modality DICOMVolume::modality() const
{
	return m_modality;
}

itk::Size<3> DICOMVolume::size() const
{
	return m_baseImage->GetLargestPossibleRegion().GetSize();
}

Vector3d DICOMVolume::spacing() const
{
	itk::Vector<double,3> v = m_baseImage->GetSpacing();
	return Vector3d(v[0], v[1], v[2]);
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
