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
VolumeChoice VolumeChoiceFile::load(const std::string& filename)
{
	std::ifstream is(filename.c_str());
	if(is.fail()) throw Exception("Could not open " + filename + " for reading");
	return VolumeChoiceSection::load(is);
}

}
