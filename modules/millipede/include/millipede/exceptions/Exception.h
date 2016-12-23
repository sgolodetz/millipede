/***
 * millipede: Exception.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_EXCEPTION
#define H_MILLIPEDE_EXCEPTION

#include <exception>
#include <string>

namespace mp {

class Exception : public std::exception
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_cause;

	//#################### CONSTRUCTORS ####################
public:
	explicit Exception(const std::string& cause)
	:	m_cause(cause)
	{}

	//#################### DESTRUCTOR ####################
public:
	virtual ~Exception() throw() {}

	//#################### PUBLIC METHODS ####################
	virtual const std::string& cause() const
	{
		return m_cause;
	}

	const char *what() const throw()
	{
		return m_cause.c_str();
	}
};

}

#endif
