/***
 * millipede: DICOMVolume.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMVOLUME
#define H_MILLIPEDE_DICOMVOLUME

#include <itkImage.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
class WindowSettings;

class DICOMVolume
{
	//#################### TYPEDEFS ####################
public:
	typedef itk::Image<int,3> BaseImage;
	typedef BaseImage::Pointer BaseImagePointer;
	typedef BaseImage::ConstPointer BaseImageCPointer;
	typedef BaseImage::SizeType Size;

	typedef itk::Image<unsigned char,3> WindowedImage;
	typedef WindowedImage::Pointer WindowedImagePointer;
	typedef WindowedImage::ConstPointer WindowedImageCPointer;

	//#################### PRIVATE VARIABLES ####################
private:
	BaseImagePointer m_baseImage;

	//#################### CONSTRUCTORS ####################
public:
	explicit DICOMVolume(const BaseImagePointer& baseImage);

	//#################### PUBLIC METHODS ####################
public:
	BaseImageCPointer base_image() const;
	Size size() const;
	WindowedImageCPointer windowed_image(const WindowSettings& windowSettings) const;
};

}

#endif
