/***
 * millipede: PipelineJob.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PIPELINEJOB
#define H_MILLIPEDE_PIPELINEJOB

#include <boost/optional.hpp>

#include "Job.h"

namespace mp {

template <typename Input, typename Output>
class PipelineJob : public Job
{
	//#################### PRIVATE VARIABLES ####################
private:
	boost::shared_ptr<boost::optional<Input> > m_input;
	boost::shared_ptr<boost::optional<Output> > m_output;

	//#################### CONSTRUCTORS ####################
public:
	PipelineJob()
	:	m_input(new boost::optional<Input>), m_output(new boost::optional<Output>)
	{}

	//#################### PUBLIC METHODS ####################
public:
	const Output& get_output() const
	{
		return **m_output;
	}

	boost::shared_ptr<boost::optional<Output> > get_output_handle() const
	{
		return m_output;
	}

	void set_input(const Input& input)
	{
		*m_input = input;
	}

	void set_input_handle(const boost::shared_ptr<boost::optional<Input> >& input)
	{
		m_input = input;
	}

	//#################### PROTECTED METHODS ####################
protected:
	const Input& get_input() const
	{
		return **m_input;
	}

	boost::shared_ptr<boost::optional<Input> > get_input_handle() const
	{
		return m_input;
	}

	void set_output(const Output& output)
	{
		*m_output = output;
	}

	void set_output_handle(const boost::shared_ptr<boost::optional<Output> >& output)
	{
		m_output = output;
	}
};

}

#endif
