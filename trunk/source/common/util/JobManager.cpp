/***
 * millipede: JobManager.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "JobManager.h"

namespace mp {

//#################### SINGLETON IMPLEMENTATION ####################
JobManager::JobManager()
{}

JobManager& JobManager::instance()
{
	static JobManager s_instance;
	return s_instance;
}

//#################### PUBLIC METHODS ####################
bool JobManager::has_jobs() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return !m_jobs.empty();
}

void JobManager::queue_job(Job *job)
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_jobs.push(Job_Ptr(job));
}

void JobManager::run_next_job()
{
	Job_Ptr job;

	// Note:	This next bit is done in its own scope to ensure that the mutex is released before the job is run.
	//			This avoids the JobManager being tied up while the job itself is running.
	{
		boost::mutex::scoped_lock lock(m_mutex);
		if(!m_jobs.empty())
		{
			job = m_jobs.front();
			m_jobs.pop();
		}
	}

	if(job) (*job)();
}

}
