/***
 * millipede: VolumeLoader.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMELOADER
#define H_MILLIPEDE_VOLUMELOADER

#include <string>

#include <boost/shared_ptr.hpp>
#include <boost/thread/mutex.hpp>
using boost::shared_ptr;

#include <itkImage.h>

#include <common/io/util/VolumeChoice.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<const class DICOMDirectory> DICOMDirectory_CPtr;
typedef shared_ptr<class Volume> Volume_Ptr;

class VolumeLoader
{
	//#################### PRIVATE VARIABLES ####################
private:
	DICOMDirectory_CPtr m_dicomdir;
	VolumeChoice m_volumeChoice;

	bool m_aborted;
	int m_max;
	mutable int m_progress;
	mutable boost::mutex m_mutex;
	std::string m_status;

	Volume_Ptr m_volume;

	//#################### CONSTRUCTORS ####################
public:
	VolumeLoader(const DICOMDirectory_CPtr& dicomdir, const VolumeChoice& volumeChoice);

	//#################### PUBLIC METHODS ####################
public:
	void abort();
	bool aborted() const;
	void load();
	int max() const;
	int progress() const;
	std::string status() const;
	const Volume_Ptr& volume();
};

}

#endif
