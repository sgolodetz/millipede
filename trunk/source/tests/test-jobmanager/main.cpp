/***
 * test-jobmanager: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>
#include <sstream>

#include <boost/thread.hpp>

#include <common/jobs/MainThreadJobQueue.h>
#include <common/jobs/SimpleJob.h>
using namespace mp;

struct TestJob : SimpleJob
{
	int m_i;

	explicit TestJob(int i) : m_i(i) {}

	void operator()()
	{
		std::ostringstream oss;
		oss << "Doing a test job in the main thread: " << m_i << '\n';
		std::cout << oss.str();
		set_finished();
	}

	int length() const	{ return 1; }
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
		MainThreadJobQueue::instance().queue_job(new TestJob(i));
	}
}

int main()
{
	boost::thread th(&f);

	MainThreadJobQueue& jobQueue = MainThreadJobQueue::instance();

	const unsigned long ITERATIONS = 20;
	for(unsigned long i=0; i<ITERATIONS; ++i)
	{
		std::ostringstream oss;
		oss << "Doing normal processing in the main thread: " << i << '\n';
		std::cout << oss.str();

		if(jobQueue.has_jobs()) jobQueue.run_next_job();
	}

	th.join();

	while(jobQueue.has_jobs()) jobQueue.run_next_job();

	return 0;
}
