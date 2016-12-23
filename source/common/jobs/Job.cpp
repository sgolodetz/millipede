/***
 * millipede: Job.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "Job.h"

#include <boost/bind.hpp>

#include "MainThreadJobQueue.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Job::Job()
:	m_aborted(false), m_mainThreadJobQueue(new MainThreadJobQueue)
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

void Job::execute_managed(const boost::shared_ptr<Job>& job)
{
	execute_in_thread(job);

	MainThreadJobQueue_Ptr mtjq = job->main_thread_job_queue();
	while(!job->is_finished())
	{
		if(job->is_aborted()) break;
		if(mtjq->has_jobs()) mtjq->run_next_job();
	}
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

MainThreadJobQueue_Ptr Job::main_thread_job_queue()
{
	return m_mainThreadJobQueue;
}

void Job::set_main_thread_job_queue(const MainThreadJobQueue_Ptr& mainThreadJobQueue)
{
	m_mainThreadJobQueue = mainThreadJobQueue;
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
