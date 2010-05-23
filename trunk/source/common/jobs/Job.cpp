/***
 * millipede: Job.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "Job.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Job::Job()
:	m_aborted(false)
{}

//#################### DESTRUCTOR ####################
Job::~Job()
{}

//#################### PUBLIC METHODS ####################
void Job::abort()
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_aborted = true;
}

bool Job::is_aborted() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_aborted;
}

bool Job::is_finished() const
{
	return progress() == length();
}

}
