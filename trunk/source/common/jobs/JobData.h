/***
 * millipede: JobData.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_JOBDATA
#define H_MILLIPEDE_JOBDATA

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>

namespace mp {

template <typename T>
class JobData
{
	//#################### PRIVATE VARIABLES ####################
private:
	boost::shared_ptr<boost::optional<T> > m_data;

	//#################### CONSTRUCTORS ####################
public:
	JobData()
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
