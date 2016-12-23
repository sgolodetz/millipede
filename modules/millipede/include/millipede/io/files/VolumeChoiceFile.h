/***
 * millipede: VolumeChoiceFile.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMECHOICEFILE
#define H_MILLIPEDE_VOLUMECHOICEFILE

#include <millipede/dicom/volumes/DICOMVolumeChoice.h>

namespace mp {

struct VolumeChoiceFile
{
	//#################### LOADING METHODS ####################
	static DICOMVolumeChoice load(const std::string& filename);

	//#################### SAVING METHODS ####################
	static void save(const std::string& filename, const DICOMVolumeChoice& volumeChoice);
};

}

#endif
