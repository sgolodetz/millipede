/***
 * millipede: SimpleJob.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SIMPLEJOB
#define H_MILLIPEDE_SIMPLEJOB

#include "Job.h"

namespace mp {

class SimpleJob : public Job
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_progress;
	std::string m_status;

	//#################### CONSTRUCTORS ####################
public:
	SimpleJob();

	//#################### PUBLIC METHODS ####################
public:
	int progress() const;
	std::string status() const;

	//#################### PROTECTED METHODS ####################
protected:
	void increment_progress();
	void set_finished();
	void set_progress(int progress);
	void set_status(const std::string& status);
};

}

#endif
