/***
 * millipede: VolumeLoader.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMELOADER
#define H_MILLIPEDE_VOLUMELOADER

#include <boost/thread/mutex.hpp>

#include <itkImage.h>

namespace mp {

class VolumeLoader
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_progress;
	boost::mutex m_progressMutex;

	//#################### PUBLIC METHODS ####################
public:
	// TODO
	int progress() const;
};

}

#endif
