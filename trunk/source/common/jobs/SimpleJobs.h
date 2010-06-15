/***
 * millipede: SimpleJobs.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SIMPLEJOBS
#define H_MILLIPEDE_SIMPLEJOBS

#include <common/util/NullType.h>
#include "PipelineJob.h"

namespace mp {

template <typename Input, typename Output>
class SimplePipelineJob : public PipelineJob<Input,Output>
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_progress;

	//#################### CONSTRUCTORS ####################
public:
	SimplePipelineJob()
	:	m_progress(0)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int progress() const
	{
		boost::mutex::scoped_lock lock(m_mutex);
		return m_progress;
	}

	std::string status() const
	{
		boost::mutex::scoped_lock lock(m_mutex);
		return m_status;
	}

	//#################### PROTECTED METHODS ####################
protected:
	void increment_progress()
	{
		boost::mutex::scoped_lock lock(m_mutex);
		++m_progress;
	}

	void set_finished()
	{
		set_progress(length());
	}

	void set_progress(int progress)
	{
		boost::mutex::scoped_lock lock(m_mutex);
		m_progress = progress;
	}
};

//#################### DERIVED TYPES ####################
typedef SimplePipelineJob<NullType,NullType> SimpleJob;
template <typename Input> struct SimpleSinkJob : SimplePipelineJob<Input,NullType> {};
template <typename Output> struct SimpleSourceJob : SimplePipelineJob<NullType,Output> {};

}

#endif
