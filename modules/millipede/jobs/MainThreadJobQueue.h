/***
 * millipede: MainThreadJobQueue.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MAINTHREADJOBQUEUE
#define H_MILLIPEDE_MAINTHREADJOBQUEUE

#include <queue>

#include <boost/thread/mutex.hpp>

#include "Job.h"

namespace mp {

class MainThreadJobQueue
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::queue<Job_Ptr> m_jobs;
	mutable boost::mutex m_mutex;

	//#################### PUBLIC METHODS ####################
public:
	void clear();
	bool has_jobs() const;
	void queue_job(Job *job);
	void queue_job(const Job_Ptr& job);
	void run_next_job();
};

}

#endif
