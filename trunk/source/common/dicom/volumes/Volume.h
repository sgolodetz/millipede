/***
 * millipede: Volume.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUME
#define H_MILLIPEDE_VOLUME

#include <itkImage.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
class WindowSettings;

class Volume
{
	//#################### TYPEDEFS ####################
public:
	typedef itk::Image<signed int,3> Image;
	typedef Image::Pointer ImagePointer;
	typedef Image::ConstPointer ImageCPointer;
	typedef Image::SizeType Size;

	//#################### PRIVATE VARIABLES ####################
private:
	ImagePointer m_baseImage;

	//#################### CONSTRUCTORS ####################
public:
	explicit Volume(const ImagePointer& baseImage);

	//#################### PUBLIC METHODS ####################
public:
	ImageCPointer base_image() const;
	Size size() const;
	ImageCPointer windowed_image(const WindowSettings& windowSettings) const;
};

}

#endif
