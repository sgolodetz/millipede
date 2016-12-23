/***
 * millipede: DICOMVolumeLoader.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMVOLUMELOADER
#define H_MILLIPEDE_DICOMVOLUMELOADER

#include <string>

#include <boost/shared_ptr.hpp>

#include <itkImage.h>

#include <millipede/dicom/volumes/DICOMVolumeChoice.h>
#include <millipede/jobs/SimpleJob.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMDirectory> DICOMDirectory_CPtr;
typedef boost::shared_ptr<class DICOMVolume> DICOMVolume_Ptr;

class DICOMVolumeLoader : public SimpleJob
{
	//#################### PRIVATE VARIABLES ####################
private:
	DICOMDirectory_CPtr m_dicomdir;
	DICOMVolume_Ptr m_volume;
	DICOMVolumeChoice m_volumeChoice;

	//#################### CONSTRUCTORS ####################
public:
	DICOMVolumeLoader(const DICOMDirectory_CPtr& dicomdir, const DICOMVolumeChoice& volumeChoice);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;
	const DICOMVolume_Ptr& volume();
	const DICOMVolumeChoice& volume_choice() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
	static std::string read_header_field(const itk::Image<int,2>::Pointer& image, const std::string& key);
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<DICOMVolumeLoader> DICOMVolumeLoader_Ptr;

}

#endif
