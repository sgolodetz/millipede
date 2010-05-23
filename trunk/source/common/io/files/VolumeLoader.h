/***
 * millipede: VolumeLoader.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMELOADER
#define H_MILLIPEDE_VOLUMELOADER

#include <string>

#include <boost/shared_ptr.hpp>

#include <itkImage.h>

#include <common/io/util/VolumeChoice.h>
#include <common/jobs/SimpleJob.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMDirectory> DICOMDirectory_CPtr;
typedef boost::shared_ptr<class Volume> Volume_Ptr;

class VolumeLoader : public SimpleJob
{
	//#################### PRIVATE VARIABLES ####################
private:
	DICOMDirectory_CPtr m_dicomdir;
	VolumeChoice m_volumeChoice;
	Volume_Ptr m_volume;

	//#################### CONSTRUCTORS ####################
public:
	VolumeLoader(const DICOMDirectory_CPtr& dicomdir, const VolumeChoice& volumeChoice);

	//#################### PUBLIC METHODS ####################
public:
	void execute();
	int length() const;
	const Volume_Ptr& volume();
	const VolumeChoice& volume_choice() const;

	//#################### PRIVATE METHODS ####################
private:
	static std::string read_header_field(const itk::Image<int,2>::Pointer& image, const std::string& key);
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<VolumeLoader> VolumeLoader_Ptr;

}

#endif
