/***
 * millipede: CompositeJob.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CompositeJob.h"

#include "MainThreadJobQueue.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CompositeJob::CompositeJob()
:	m_length(0), m_progress(0)
{}

//#################### PUBLIC OPERATORS ####################
void CompositeJob::operator()()
{
	// Note:	Composite jobs with sub-jobs that have been added via add_main_thread_job() MUST be run in their own thread.
	//			(More precisely, they must be run in a separate thread from the one running the main thread job queue.)
	//			If they are run in the main thread, the program will hang, because the loop below will keep waiting for the
	//			sub-jobs to complete, and this will never happen while the main thread job queue is stalled.

	for(size_t i=0, size=m_jobs.size(); i<size; ++i)
	{
		// Set the pointer to the current job. Note that this is the only place where the pointer is modified, so
		// we don't need to hang onto the mutex beyond this (we know the pointer won't be modified elsewhere).
		// This only works because boost::shared_ptr is thread-safe for simultaneous reads by multiple threads.
		{
			boost::mutex::scoped_lock lock(m_mutex);
			m_currentJob = m_jobs[i].first;
		}

		if(m_jobs[i].second) MainThreadJobQueue::instance().queue_job(m_currentJob);
		else (*m_currentJob)();

		while(!m_currentJob->is_finished());
	}
}

//#################### PUBLIC METHODS ####################
void CompositeJob::abort()
{
	Job::abort();

	boost::mutex::scoped_lock lock(m_mutex);
	m_currentJob->abort();
}

void CompositeJob::add_job(const Job_Ptr& job)
{
	m_jobs.push_back(std::make_pair(job, false));
}

void CompositeJob::add_main_thread_job(const Job_Ptr& job)
{
	m_jobs.push_back(std::make_pair(job, true));
}

int CompositeJob::length() const
{
	return m_length;
}

int CompositeJob::progress() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_progress + m_currentJob->progress();
}

std::string CompositeJob::status() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_currentJob->status();
}

}
