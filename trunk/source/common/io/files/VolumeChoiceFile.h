/***
 * millipede: VolumeChoiceFile.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMECHOICEFILE
#define H_MILLIPEDE_VOLUMECHOICEFILE

#include <common/io/util/VolumeChoice.h>

namespace mp {

struct VolumeChoiceFile
{
	//#################### LOADING METHODS ####################
	static VolumeChoice load(const std::string& filename);
};

}

#endif
