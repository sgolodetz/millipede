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

boost::shared_ptr<boost::thread> Job::execute_in_thread(const boost::shared_ptr<Job>& job)
{
	return boost::shared_ptr<boost::thread>(new boost::thread(boost::bind(&Job::safe_job_executor, job)));
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

//#################### PROTECTED METHODS ####################
void Job::set_status(const std::string& status)
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_status = status;
}

//#################### PRIVATE METHODS ####################
void Job::safe_job_executor(const boost::shared_ptr<Job>& job)
{
	try
	{
		job->execute();
	}
	catch(std::exception& e)
	{
		job->set_status(e.what());
		job->abort();
	}
}

}
