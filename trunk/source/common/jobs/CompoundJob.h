/***
 * millipede: CompoundJob.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_COMPOUNDJOB
#define H_MILLIPEDE_COMPOUNDJOB

#include "Job.h"
#include <vector>

namespace mp {

class CompoundJob : public virtual Job
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_progress;
	std::vector<Job> jobs;

	//#################### CONSTRUCTORS ####################
public:
	CompoundJob();

	//#################### PRIVATE ABSTRACT METHODS ####################
private:
	virtual void execute_impl() = 0;

	//#################### PUBLIC METHODS ####################
public:
	void add(Job& job);
	void execute();
	int progress() const;
	std::string status() const;

	//#################### PROTECTED METHODS ####################
protected:
	void increment_progress();
	void set_progress(int progress);
};

}

#endif
