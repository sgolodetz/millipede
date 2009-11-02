/***
 * millipede: VolumeLoader.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeLoader.h"

#include <boost/thread/mutex.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
VolumeLoader::VolumeLoader()
:	m_aborted(false)
{}

//#################### PUBLIC METHODS ####################
void VolumeLoader::abort()
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_aborted = true;
}

bool VolumeLoader::aborted() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_aborted;
}

void VolumeLoader::load()
{
	// NYI
	const int END = 10000000;
	for(int i=0; i<=END; ++i)
	{
		boost::mutex::scoped_lock lock(m_mutex);
		if(!m_aborted) m_progress = max()*i/END;
		else break;
	}
}

int VolumeLoader::max() const
{
	// FIXME
	return 100;
}

int VolumeLoader::progress() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_progress;
}

std::string VolumeLoader::status() const
{
	return "Doing something...";
}

}
