/***
 * millipede: DICOMVolume.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "dicom/volumes/DICOMVolume.h"

#include <itkCastImageFilter.h>
#include <itkIntensityWindowingImageFilter.h>
#include <itkJoinSeriesImageFilter.h>

#include "dicom/util/WindowSettings.h"

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMVolume::DICOMVolume(const BaseImagePointer& baseImage, Modality modality)
:	m_baseImage(baseImage), m_modality(modality), m_windowedImage(NULL)
{}

DICOMVolume::DICOMVolume(const WindowedImagePointer& windowedImage)
: m_modality(STANDALONE), m_windowedImage(windowedImage)
{
	typedef itk::CastImageFilter<WindowedImage,BaseImage> CastFilter;
	CastFilter::Pointer castFilter = CastFilter::New();
	castFilter->SetInput(windowedImage);
	castFilter->Update();
	m_baseImage = castFilter->GetOutput();
}

//#################### PUBLIC METHODS ####################
DICOMVolume::BaseImagePointer DICOMVolume::base_image() const
{
	return m_baseImage;
}

DICOMVolume::Modality DICOMVolume::modality() const
{
	return m_modality;
}

Vector3d DICOMVolume::origin() const
{
	itk::Point<double,3> p = m_baseImage->GetOrigin();
	return Vector3d(p[0], p[1], p[2]);
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

double DICOMVolume::voxel_size_mm3() const
{
	itk::Vector<double,3> v = m_baseImage->GetSpacing();
	return v[0] * v[1] * v[2];
}

DICOMVolume::WindowedImagePointer DICOMVolume::windowed_image(const WindowSettings& windowSettings) const
{
	if(m_windowedImage)
	{
		return m_windowedImage;
	}
	else
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

}
