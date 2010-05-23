/***
 * millipede: Job.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_JOB
#define H_MILLIPEDE_JOB

#include <string>

#include <boost/shared_ptr.hpp>
#include <boost/thread/mutex.hpp>

namespace mp {

class Job
{
	//#################### PRIVATE VARIABLES ####################
private:
	bool m_aborted;

	//#################### PROTECTED VARIABLES ####################
protected:
	mutable boost::mutex m_mutex;

	//#################### CONSTRUCTORS ####################
public:
	Job();

	//#################### DESTRUCTOR ####################
public:
	virtual ~Job();

	//#################### PUBLIC ABSTRACT OPERATORS ####################
public:
	virtual void operator()() = 0;

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual int length() const = 0;
	virtual int progress() const = 0;
	virtual std::string status() const = 0;

	//#################### PUBLIC METHODS ####################
public:
	virtual void abort();
	bool is_aborted() const;
	bool is_finished() const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<Job> Job_Ptr;

}

#endif
