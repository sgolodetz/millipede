/***
 * millipede: OSSWrapper.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_OSSWRAPPER
#define H_MILLIPEDE_OSSWRAPPER

#include <sstream>
#include <string>

namespace mp {

/**
@brief	An OSSWrapper wraps a std::ostringstream, making it easier to construct strings on-the-fly.
		This is particularly useful when constructing exception arguments.

Example Usage: throw Exception(OSSWrapper() << "This number is invalid: " << n);
*/
class OSSWrapper
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::ostringstream m_os;

	//#################### PUBLIC OPERATORS ####################
public:
	operator std::string() const;

	template <typename T>
	OSSWrapper& operator<<(const T& rhs)
	{
		m_os << rhs;
		return *this;
	}

	OSSWrapper& operator<<(const char *rhs);
	OSSWrapper& operator<<(std::ostream& (*manip)(std::ostream&));
	OSSWrapper& operator<<(std::ios& (*manip)(std::ios&));
};

}

#endif
