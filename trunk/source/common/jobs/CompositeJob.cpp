/***
 * millipede: CompositeJob.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CompositeJob.h"

#include <boost/bind.hpp>

#include "MainThreadJobQueue.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CompositeJob::CompositeJob()
:	m_length(0), m_progress(0)
{}

//#################### PUBLIC METHODS ####################
void CompositeJob::abort()
{
	Job::abort();

	boost::mutex::scoped_lock lock(m_mutex);
	if(m_currentJob) m_currentJob->abort();
}

void CompositeJob::add_subjob(Job *job)
{
	add_subjob(Job_Ptr(job));
}

void CompositeJob::add_subjob(const Job_Ptr& job)
{
	m_jobs.push_back(std::make_pair(job, false));
	m_length += job->length();
}

void CompositeJob::add_main_thread_subjob(Job *job)
{
	add_main_thread_subjob(Job_Ptr(job));
}

void CompositeJob::add_main_thread_subjob(const Job_Ptr& job)
{
	m_jobs.push_back(std::make_pair(job, true));
	m_length += job->length();
}

void CompositeJob::execute()
{
	// Note:	Composite jobs with sub-jobs that have been added via add_main_thread_job() MUST be run in their own thread.
	//			(More precisely, they must be run in a separate thread from the one running the main thread job queue.)
	//			If they are run in the main thread, the program will hang, because the loop below will keep waiting for the
	//			sub-jobs to complete, and this will never happen while the main thread job queue is stalled. In order to
	//			avoid problems, it is generally best to run composite jobs using execute_in_thread().

	for(size_t i=0, size=m_jobs.size(); i<size && !is_aborted(); ++i)
	{
		// Set the pointer to the current job. Note that this is the only method in which the pointer is modified,
		// so we only need to acquire a mutex whilst actually modifying the pointer (we know it won't be changed
		// elsewhere). Note that boost::shared_ptr is thread-safe for simultaneous reads by multiple threads.
		{
			boost::mutex::scoped_lock lock(m_mutex);
			m_currentJob = m_jobs[i].first;
		}

		if(m_jobs[i].second) MainThreadJobQueue::instance().queue_job(m_currentJob);
		else m_currentJob->execute();

		while(!m_currentJob->is_aborted() && !m_currentJob->is_finished());

		if(m_currentJob->is_finished())
		{
			boost::mutex::scoped_lock lock(m_mutex);
			int curLength = m_currentJob->length();

			// It is crucial to reset the current job pointer here, because the progress calculation
			// would otherwise incorrectly make use of the "current" job's progress.
			m_currentJob.reset();

			m_progress += curLength;
		}
	}
}

int CompositeJob::length() const
{
	return m_length;
}

int CompositeJob::progress() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_progress + (m_currentJob ? m_currentJob->progress() : 0);
}

std::string CompositeJob::status() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_currentJob ? m_currentJob->status() : "";
}

}
