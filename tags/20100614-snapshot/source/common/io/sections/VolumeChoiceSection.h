/***
 * millipede: VolumeChoiceSection.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMECHOICESECTION
#define H_MILLIPEDE_VOLUMECHOICESECTION

#include <iosfwd>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
struct DICOMVolumeChoice;

struct VolumeChoiceSection
{
	//#################### LOADING METHODS ####################
	static DICOMVolumeChoice load(std::istream& is);

	//#################### SAVING METHODS ####################
	static void save(std::ostream& os, const DICOMVolumeChoice& volumeChoice);
};

}

#endif
