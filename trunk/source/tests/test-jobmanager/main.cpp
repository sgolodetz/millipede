/***
 * test-jobmanager: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>
#include <sstream>

#include <boost/thread.hpp>

#include <common/util/JobManager.h>
using namespace mp;

struct SimpleJob : JobManager::Job
{
	int m_i;

	explicit SimpleJob(int i) : m_i(i) {}

	void operator()()
	{
		std::ostringstream oss;
		oss << "Doing a simple job in the main thread: " << m_i << '\n';
		std::cout << oss.str();
	}
};

void f()
{
	const unsigned long ITERATIONS = 10;
	for(unsigned long i=0; i<ITERATIONS; ++i)
	{
		std::ostringstream oss;
		oss << "Doing something long and complicated in a thread: " << i << '\n';
		std::cout << oss.str();
	}

	for(int i=0; i<10; ++i)
	{
		JobManager::instance().queue_job(new SimpleJob(i));
	}
}

int main()
{
	boost::thread th(&f);

	JobManager& jobManager = JobManager::instance();

	const unsigned long ITERATIONS = 20;
	for(unsigned long i=0; i<ITERATIONS; ++i)
	{
		std::ostringstream oss;
		oss << "Doing normal processing in the main thread: " << i << '\n';
		std::cout << oss.str();

		if(jobManager.has_jobs()) jobManager.run_next_job();
	}

	th.join();

	while(jobManager.has_jobs()) jobManager.run_next_job();

	return 0;
}
