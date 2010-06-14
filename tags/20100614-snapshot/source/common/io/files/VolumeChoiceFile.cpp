/***
 * millipede: VolumeChoiceFile.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeChoiceFile.h"

#include <fstream>

#include <common/exceptions/Exception.h>
#include <common/io/sections/VolumeChoiceSection.h>

namespace mp {

//#################### LOADING METHODS ####################
DICOMVolumeChoice VolumeChoiceFile::load(const std::string& filename)
{
	std::ifstream is(filename.c_str(), std::ios_base::binary);
	if(is.fail()) throw Exception("Could not open " + filename + " for reading");
	return VolumeChoiceSection::load(is);
}

//#################### SAVING METHODS ####################
void VolumeChoiceFile::save(const std::string& filename, const DICOMVolumeChoice& volumeChoice)
{
	std::ofstream os(filename.c_str(), std::ios_base::binary);
	if(os.fail()) throw Exception("Could not open " + filename + " for writing");
	VolumeChoiceSection::save(os, volumeChoice);
}

}
