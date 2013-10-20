/***
 * millipede: DICOMPixelProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 * Modified by Varduhi Yeghiazaryan, 2013.
 ***/

#ifndef H_MILLIPEDE_DICOMPIXELPROPERTIES
#define H_MILLIPEDE_DICOMPIXELPROPERTIES

#include <iosfwd>
#include <map>
#include <string>

namespace mp {

class DICOMPixelProperties
{
	//#################### FRIENDS ####################
	friend std::istream& operator>>(std::istream& is, DICOMPixelProperties& rhs);

	//#################### PRIVATE VARIABLES ####################
private:
	// Note: Deliberately listed in non-increasing order of data size to try and minimize padding.
	int m_baseValue;
	short m_gradientMagnitudeValue;
	unsigned char m_greyValue;

	//#################### CONSTRUCTORS ####################
public:
	DICOMPixelProperties();
	DICOMPixelProperties(int baseValue, short gradientMagnitudeValue, unsigned char greyValue);

	//#################### PUBLIC METHODS ####################
public:
	int base_value() const;
	std::map<std::string,std::string> branch_property_map() const;
	short gradient_magnitude_value() const;
	unsigned char grey_value() const;
};

//#################### GLOBAL OPERATORS ####################
std::istream& operator>>(std::istream& is, DICOMPixelProperties& rhs);
std::ostream& operator<<(std::ostream& os, const DICOMPixelProperties& rhs);

}

#endif
