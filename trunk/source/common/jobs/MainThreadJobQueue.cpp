/***
 * millipede: MainThreadJobQueue.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MainThreadJobQueue.h"

namespace mp {

//#################### PUBLIC METHODS ####################
void MainThreadJobQueue::clear()
{
	m_jobs = std::queue<Job_Ptr>();
}

bool MainThreadJobQueue::has_jobs() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return !m_jobs.empty();
}

void MainThreadJobQueue::queue_job(Job *job)
{
	queue_job(Job_Ptr(job));
}

void MainThreadJobQueue::queue_job(const Job_Ptr& job)
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_jobs.push(job);
}

void MainThreadJobQueue::run_next_job()
{
	Job_Ptr job;

	// Note:	This next bit is done in its own scope to ensure that the mutex is released before the job is run.
	//			This avoids the MainThreadJobQueue being tied up while the job itself is running.
	{
		boost::mutex::scoped_lock lock(m_mutex);
		if(!m_jobs.empty())
		{
			job = m_jobs.front();
			m_jobs.pop();
		}
	}

	if(job) job->execute();
}

}
