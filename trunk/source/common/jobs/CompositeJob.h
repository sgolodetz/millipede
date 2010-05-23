/***
 * millipede: CompositeJob.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_COMPOSITEJOB
#define H_MILLIPEDE_COMPOSITEJOB

#include <utility>
#include <vector>

#include "Job.h"

namespace mp {

class CompositeJob : public Job
{
	//#################### PRIVATE VARIABLES ####################
private:
	Job_Ptr m_currentJob;
	std::vector<std::pair<Job_Ptr,bool> > m_jobs;
	int m_length;
	int m_progress;

	//#################### CONSTRUCTORS ####################
public:
	CompositeJob();

	//#################### PUBLIC OPERATORS ####################
public:
	void operator()();

	//#################### PUBLIC METHODS ####################
public:
	void abort();
	void add_job(const Job_Ptr& job);
	void add_main_thread_job(const Job_Ptr& job);
	int length() const;
	int progress() const;
	std::string status() const;
};

}

#endif
