/***
 * millipede: Job.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_JOB
#define H_MILLIPEDE_JOB

#include <string>

#include <boost/shared_ptr.hpp>
#include <boost/thread.hpp>
#include <boost/thread/mutex.hpp>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class MainThreadJobQueue> MainThreadJobQueue_Ptr;

class Job
{
	//#################### PRIVATE VARIABLES ####################
private:
	bool m_aborted;
	MainThreadJobQueue_Ptr m_mainThreadJobQueue;

	//#################### PROTECTED VARIABLES ####################
protected:
	mutable boost::mutex m_mutex;
	std::string m_status;

	//#################### CONSTRUCTORS ####################
public:
	Job();

	//#################### DESTRUCTOR ####################
public:
	virtual ~Job();

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void execute() = 0;
	virtual int length() const = 0;
	virtual int progress() const = 0;
	virtual std::string status() const = 0;

	//#################### PUBLIC METHODS ####################
public:
	virtual void abort();
	static boost::shared_ptr<boost::thread> execute_in_thread(const boost::shared_ptr<Job>& job);
	bool is_aborted() const;
	bool is_finished() const;
	MainThreadJobQueue_Ptr main_thread_job_queue();
	virtual void set_main_thread_job_queue(const MainThreadJobQueue_Ptr& mainThreadJobQueue);

	//#################### PROTECTED METHODS ####################
protected:
	void set_status(const std::string& status);

	//#################### PRIVATE METHODS ####################
private:
	static void safe_job_executor(const boost::shared_ptr<Job>& job);
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<Job> Job_Ptr;

}

#endif
