/***
 * millipede: JobManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_JOBMANAGER
#define H_MILLIPEDE_JOBMANAGER

#include <queue>

#include <boost/shared_ptr.hpp>
#include <boost/thread/mutex.hpp>

namespace mp {

class JobManager
{
	//#################### NESTED CLASSES ####################
public:
	struct Job
	{
		virtual ~Job() {}
		virtual void operator()() = 0;
	};

	//#################### TYPEDEFS ####################
public:
	typedef boost::shared_ptr<Job> Job_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	std::queue<Job_Ptr> m_jobs;
	mutable boost::mutex m_mutex;

	//#################### SINGLETON IMPLEMENTATION ####################
private:
	JobManager();
public:
	static JobManager& instance();

	//#################### PUBLIC METHODS ####################
public:
	bool has_jobs() const;
	void queue_job(Job *job);
	void run_next_job();
};

}

#endif
