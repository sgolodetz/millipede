/***
 * millipede: SimplePixelProperties.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SIMPLEPIXELPROPERTIES
#define H_MILLIPEDE_SIMPLEPIXELPROPERTIES

#include <iosfwd>

namespace mp {

class SimplePixelProperties
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_value;

	//#################### CONSTRUCTORS ####################
public:
	SimplePixelProperties(int value);

	//#################### PUBLIC METHODS ####################
public:
	int value() const;
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const SimplePixelProperties& rhs);

}

#endif
