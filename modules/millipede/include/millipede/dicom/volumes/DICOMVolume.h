/***
 * millipede: DICOMVolume.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMVOLUME
#define H_MILLIPEDE_DICOMVOLUME

#include <boost/shared_ptr.hpp>

#include <itkImage.h>

#include "../../math/Vector3.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
class WindowSettings;

class DICOMVolume
{
	//#################### CONSTANTS ####################
public:
	enum Modality
	{
		UNSUPPORTED_MODALITY,
		CT,
		MR,
		STANDALONE,
	};

	//#################### TYPEDEFS ####################
public:
	typedef itk::Image<int,3> BaseImage;
	typedef BaseImage::Pointer BaseImagePointer;

	typedef itk::Image<unsigned char,3> WindowedImage;
	typedef WindowedImage::Pointer WindowedImagePointer;

	//#################### PRIVATE VARIABLES ####################
private:
	BaseImagePointer m_baseImage;
	Modality m_modality;
	WindowedImagePointer m_windowedImage; // only cached for standalone images

	//#################### CONSTRUCTORS ####################
public:
	DICOMVolume(const BaseImagePointer& baseImage, Modality modality);
	explicit DICOMVolume(const WindowedImagePointer& windowedImage);

	//#################### PUBLIC METHODS ####################
public:
	BaseImagePointer base_image() const;
	Modality modality() const;
	Vector3d origin() const;
	itk::Size<3> size() const;
	Vector3d spacing() const;
	double voxel_size_mm3() const;
	WindowedImagePointer windowed_image(const WindowSettings& windowSettings) const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<DICOMVolume> DICOMVolume_Ptr;
typedef boost::shared_ptr<const DICOMVolume> DICOMVolume_CPtr;

}

#endif
