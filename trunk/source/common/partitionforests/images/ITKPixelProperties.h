/***
 * millipede: ITKPixelProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ITKPIXELPROPERTIES
#define H_MILLIPEDE_ITKPIXELPROPERTIES

namespace mp {

class ITKPixelProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	unsigned char m_greyValue;
	signed int m_hounsfieldValue;

	//#################### CONSTRUCTORS ####################
public:
	ITKPixelProperties(unsigned char greyValue, signed int hounsfieldValue);

	//#################### PUBLIC METHODS ####################
public:
	unsigned char grey_value() const;
	signed int hounsfield_value() const;
};

}

#endif
