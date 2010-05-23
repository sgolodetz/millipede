/***
 * millipede: MainThreadJobQueue.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MAINTHREADJOBQUEUE
#define H_MILLIPEDE_MAINTHREADJOBQUEUE

#include <queue>

#include <boost/shared_ptr.hpp>
#include <boost/thread/mutex.hpp>

namespace mp {

class MainThreadJobQueue
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
	MainThreadJobQueue();
public:
	static MainThreadJobQueue& instance();

	//#################### PUBLIC METHODS ####################
public:
	bool has_jobs() const;
	void queue_job(Job *job);
	void run_next_job();
};

}

#endif
