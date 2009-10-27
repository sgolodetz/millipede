/***
 * millipede: VolumeChoiceSection.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMECHOICESECTION
#define H_MILLIPEDE_VOLUMECHOICESECTION

#include <common/io/util/VolumeChoice.h>

namespace mp {

struct VolumeChoiceSection
{
	//#################### LOADING METHODS ####################
	static VolumeChoice load(std::istream& is);
};

}

#endif
