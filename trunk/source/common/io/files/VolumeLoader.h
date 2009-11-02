/***
 * millipede: VolumeLoader.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMELOADER
#define H_MILLIPEDE_VOLUMELOADER

#include <string>

#include <boost/thread/mutex.hpp>

#include <itkImage.h>

namespace mp {

class VolumeLoader
{
	//#################### PRIVATE VARIABLES ####################
private:
	bool m_aborted;
	int m_max;
	mutable int m_progress;
	mutable boost::mutex m_mutex;

	//#################### CONSTRUCTORS ####################
public:
	VolumeLoader();

	//#################### PUBLIC METHODS ####################
public:
	void abort();
	bool aborted() const;
	// FIXME
	void load();
	int max() const;
	int progress() const;
	std::string status() const;
};

}

#endif
