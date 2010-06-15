/***
 * millipede: CompositeJobs.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_COMPOSITEJOBS
#define H_MILLIPEDE_COMPOSITEJOBS

#include <utility>
#include <vector>

#include <common/util/NullType.h>
#include "MainThreadJobQueue.h"
#include "PipelineJob.h"

namespace mp {

enum JobThreadSpecifier
{
	JTS_ANYTHREAD,
	JTS_MAINTHREAD,
};

template <typename Input, typename Output>
class CompositePipelineJob : public PipelineJob<Input,Output>
{
	//#################### PRIVATE VARIABLES ####################
private:
	Job_Ptr m_currentJob;
	std::vector<std::pair<Job_Ptr,JobThreadSpecifier> > m_jobs;
	int m_length;
	int m_progress;

	//#################### CONSTRUCTORS ####################
public:
	CompositePipelineJob()
	:	m_length(0), m_progress(0)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void abort()
	{
		Job::abort();

		boost::mutex::scoped_lock lock(m_mutex);
		if(m_currentJob) m_currentJob->abort();
	}

	template <typename OtherOutput>
	void add_input_subjob(PipelineJob<Input,OtherOutput> *job, JobThreadSpecifier threadSpecifier = JTS_ANYTHREAD)
	{
		add_input_subjob(boost::shared_ptr<PipelineJob<Input,OtherOutput> >(job), threadSpecifier);
	}

	template <typename OtherOutput>
	void add_input_subjob(const boost::shared_ptr<PipelineJob<Input,OtherOutput> >& job, JobThreadSpecifier threadSpecifier = JTS_ANYTHREAD)
	{
		job->set_input_handle(get_input_handle());
		add_subjob(job, threadSpecifier);
	}

	template <typename OtherInput>
	void add_output_subjob(PipelineJob<OtherInput,Output> *job, JobThreadSpecifier threadSpecifier = JTS_ANYTHREAD)
	{
		add_output_subjob(boost::shared_ptr<PipelineJob<OtherInput,Output> >(job), threadSpecifier);
	}

	template <typename OtherInput>
	void add_output_subjob(const boost::shared_ptr<PipelineJob<OtherInput,Output> >& job, JobThreadSpecifier threadSpecifier = JTS_ANYTHREAD)
	{
		set_output_handle(job->get_output_handle());
		add_subjob(job, threadSpecifier);
	}

	void add_subjob(Job *job, JobThreadSpecifier threadSpecifier = JTS_ANYTHREAD)
	{
		add_subjob(Job_Ptr(job), threadSpecifier);
	}

	void add_subjob(const Job_Ptr& job, JobThreadSpecifier threadSpecifier = JTS_ANYTHREAD)
	{
		m_jobs.push_back(std::make_pair(job, threadSpecifier));
		m_length += job->length();
		job->set_main_thread_job_queue(main_thread_job_queue());
	}

	void execute()
	{
		// Note:	Composite jobs with main-thread-only sub-jobs MUST be run in their own thread. (More precisely, they must be
		//			run in a separate thread from the one running the main thread job queue.) If they are run in the main thread,
		//			the program will hang, because the loop below will keep waiting for the sub-jobs to complete, and this will
		//			never happen while the main thread job queue is stalled. In order to avoid problems, it is generally best to
		//			run composite jobs using Job::execute_in_thread().

		for(size_t i=0, size=m_jobs.size(); i<size && !is_aborted(); ++i)
		{
			// Set the pointer to the current job. Note that this is the only method in which the pointer is modified,
			// so we only need to acquire a mutex whilst actually modifying the pointer (we know it won't be changed
			// elsewhere). Note that boost::shared_ptr is thread-safe for simultaneous reads by multiple threads.
			{
				boost::mutex::scoped_lock lock(m_mutex);
				m_currentJob = m_jobs[i].first;
			}

			if(m_jobs[i].second == JTS_MAINTHREAD) main_thread_job_queue()->queue_job(m_currentJob);
			else m_currentJob->execute();

			while(!m_currentJob->is_aborted() && !m_currentJob->is_finished());

			if(m_currentJob->is_finished())
			{
				boost::mutex::scoped_lock lock(m_mutex);
				int curLength = m_currentJob->length();

				// It is crucial to reset the current job pointer here, because the progress calculation
				// would otherwise incorrectly make use of the "current" job's progress.
				m_currentJob.reset();

				m_progress += curLength;
			}
			else if(m_currentJob->is_aborted())
			{
				boost::mutex::scoped_lock lock(m_mutex);
				m_currentJob.reset();
			}
		}
	}

	int length() const
	{
		return m_length;
	}

	int progress() const
	{
		boost::mutex::scoped_lock lock(m_mutex);
		return m_progress + (m_currentJob && !m_currentJob->is_aborted() ? m_currentJob->progress() : 0);
	}

	void set_main_thread_job_queue(const MainThreadJobQueue_Ptr& mainThreadJobQueue)
	{
		Job::set_main_thread_job_queue(mainThreadJobQueue);
		for(size_t i=0, size=m_jobs.size(); i<size; ++i)
		{
			m_jobs[i].first->set_main_thread_job_queue(mainThreadJobQueue);
		}
	}

	std::string status() const
	{
		boost::mutex::scoped_lock lock(m_mutex);
		return m_currentJob && !m_currentJob->is_aborted() ? m_currentJob->status() : m_status;
	}
};

//#################### DERIVED TYPES ####################
typedef CompositePipelineJob<NullType,NullType> CompositeJob;
template <typename Input> struct CompositeSinkJob : CompositePipelineJob<Input,NullType> {};
template <typename Output> struct CompositeSourceJob : CompositePipelineJob<NullType,Output> {};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<CompositeJob> CompositeJob_Ptr;

}

#endif
