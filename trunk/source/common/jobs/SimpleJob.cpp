/***
 * millipede: SimpleJob.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SimpleJob.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SimpleJob::SimpleJob()
:	m_finished(false)
{}

//#################### PUBLIC METHODS ####################
int SimpleJob::progress() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	if(m_finished)	return length();
	else			return 0;
}

std::string SimpleJob::status() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_status;
}

//#################### PROTECTED METHODS ####################
void SimpleJob::set_finished()
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_finished = true;
}

void SimpleJob::set_status(const std::string& status)
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_status = status;
}

}
