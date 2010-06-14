/***
 * millipede: OSSWrapper.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "OSSWrapper.h"

namespace mp {

//#################### PUBLIC OPERATORS ####################
OSSWrapper::operator std::string() const
{
	return m_os.str();
}

OSSWrapper& OSSWrapper::operator<<(const char *rhs)
{
	m_os << rhs;
	return *this;
}

OSSWrapper& OSSWrapper::operator<<(std::ostream& (*manip)(std::ostream&))
{
	m_os << manip;
	return *this;
}

OSSWrapper& OSSWrapper::operator<<(std::ios& (*manip)(std::ios&))
{
	m_os << manip;
	return *this;
}

}
