/***
 * test-jobs: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>
#include <sstream>

#include <boost/thread.hpp>

#include <common/io/util/OSSWrapper.h>
#include <common/jobs/CompositeJob.h>
#include <common/jobs/MainThreadJobQueue.h>
#include <common/jobs/SimpleJob.h>
using namespace mp;

//#################### TEST 1 ####################
struct TestJob : SimpleJob
{
	int m_i;

	explicit TestJob(int i) : m_i(i) {}

	void execute()
	{
		std::ostringstream oss;
		oss << "Doing a test job in the main thread: " << m_i << '\n';
		std::cout << oss.str();
		set_finished();
	}

	int length() const
	{
		return 1;
	}
};

void f(MainThreadJobQueue& jobQueue)
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
		jobQueue.queue_job(new TestJob(i));
	}
}

void test1()
{
	MainThreadJobQueue jobQueue;
	boost::thread th(boost::bind(&f, boost::ref(jobQueue)));

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
}

//#################### TEST 2 ####################
struct MainThreadJob : SimpleJob
{
	void execute()
	{
		// Note: We can't do anything too interesting in a main thread job without locking up the user interface.
		int k = 23 * 9;
		std::cout << k << '\n';
		set_finished();
	}

	int length() const
	{
		return 1;
	}
};

struct OtherThreadJob : SimpleJob
{
	int m_index;

	explicit OtherThreadJob(int index)
	:	m_index(index)
	{}

	void execute()
	{
		std::cout << "[Other Thread] Executing Sub-Job\n";

		// Do something non-trivial to make the job take a little bit of time.
		for(int i=1; i<=10000; ++i)
		{
			for(int j=0; j<10000; ++j)
			{
				int k = i*j;
			}

			if(i % 1000 == 0)
			{
				set_progress(i / 1000);
				set_status(OSSWrapper() << '(' << m_index << ") Finished iteration " << i);
			}
		}

		set_finished();
	}

	int length() const
	{
		return 10;
	}
};

boost::shared_ptr<Job> construct_job()
{
	boost::shared_ptr<CompositeJob> job(new CompositeJob);
	job->add_subjob(new OtherThreadJob(0));
	job->add_main_thread_subjob(new MainThreadJob);
	job->add_subjob(new OtherThreadJob(1));
	return job;
}

void show_progress(const boost::shared_ptr<Job>& job)
{
	MainThreadJobQueue_Ptr mtjq = job->main_thread_job_queue();

	int length = job->length();
	int lastProgress = -1;
	std::string lastStatus = "";
	while(!job->is_aborted() && !job->is_finished())
	{
		int progress = job->progress();
		std::string status = job->status();

		if(progress != lastProgress)
		{
			std::ostringstream oss;
			oss << "[Progress Update] Done " << progress << " of " << length << '\n';
			std::cout << oss.str();
			lastProgress = progress;
		}

		if(status != lastStatus)
		{
			std::ostringstream oss;
			oss << "[Status Update] " << status << '\n';
			std::cout << oss.str();
			lastStatus = status;

			if(status == "(1) Finished iteration 2000")
			{
				job->abort();
				continue;
			}
		}

		if(mtjq->has_jobs())
		{
			std::cout << "[Main Thread] Executing Sub-Job\n";
			mtjq->run_next_job();
		}
	}

	if(job->is_finished())	std::cout << "[Progress Update] Finished\n";
	else					std::cout << "[Progress Update] Aborted\n";
}

void test2()
{
	boost::shared_ptr<Job> job = construct_job();
	Job::execute_in_thread(job);
	show_progress(job);
}

int main()
{
	//test1();
	test2();
	return 0;
}
