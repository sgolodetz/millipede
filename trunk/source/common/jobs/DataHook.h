/***
 * millipede: DataHook.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DATAHOOK
#define H_MILLIPEDE_DATAHOOK

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>

namespace mp {

template <typename T>
class DataHook
{
	//#################### PRIVATE VARIABLES ####################
private:
	boost::shared_ptr<boost::optional<T> > m_data;

	//#################### CONSTRUCTORS ####################
public:
	DataHook()
	:	m_data(new boost::optional<T>)
	{}

	//#################### PUBLIC METHODS ####################
public:
	const T& get() const
	{
		return **m_data;
	}

	void set(const T& data)
	{
		*m_data = data;
	}
};

}

#endif
