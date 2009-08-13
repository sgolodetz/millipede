/***
 * millipede: Exception.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_EXCEPTION
#define H_MILLIPEDE_EXCEPTION

#include <string>

namespace mp {

class Exception
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
	virtual ~Exception() {}

	//#################### PUBLIC METHODS ####################
	virtual const std::string& cause() const
	{
		return m_cause;
	}
};

}

#endif
