/***
 * millipede: VolumeIPFFIle.cpp
 * Added by Varduhi Yeghiazaryan, 2013.
 ***/

#include "VolumeIPFFile.h"

#include <fstream>

#include <common/exceptions/Exception.h>
#include <common/io/sections/VolumeIPFSection.h>

namespace mp {

//#################### LOADING METHODS ####################
VolumeIPFFile::VolumeIPF_Ptr VolumeIPFFile::load(const std::string& filename)
{
	std::ifstream is(filename.c_str(), std::ios_base::binary);
	if(is.fail()) throw Exception("Could not open " + filename + " for reading");
	return VolumeIPFSection::load(is);
}

//#################### SAVING METHODS ####################
void VolumeIPFFile::save(const std::string& filename, const VolumeIPFFile::VolumeIPF_Ptr& volumeIPF)
{
	std::ofstream os(filename.c_str(), std::ios_base::binary);
	if(os.fail()) throw Exception("Could not open " + filename + " for writing");
	VolumeIPFSection::save(os, volumeIPF);
}

}

