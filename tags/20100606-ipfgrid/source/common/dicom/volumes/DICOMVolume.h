/***
 * millipede: DICOMVolume.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMVOLUME
#define H_MILLIPEDE_DICOMVOLUME

#include <boost/shared_ptr.hpp>

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
	typedef BaseImage::SizeType Size;

	typedef itk::Image<unsigned char,3> WindowedImage;
	typedef WindowedImage::Pointer WindowedImagePointer;

	//#################### PRIVATE VARIABLES ####################
private:
	BaseImagePointer m_baseImage;

	//#################### CONSTRUCTORS ####################
public:
	explicit DICOMVolume(const BaseImagePointer& baseImage);

	//#################### PUBLIC METHODS ####################
public:
	BaseImagePointer base_image() const;
	Size size() const;
	WindowedImagePointer windowed_image(const WindowSettings& windowSettings) const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<DICOMVolume> DICOMVolume_Ptr;
typedef boost::shared_ptr<const DICOMVolume> DICOMVolume_CPtr;

}

#endif
