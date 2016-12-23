/***
 * millipede: DICOMVolumeChoice.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMVOLUMECHOICE
#define H_MILLIPEDE_DICOMVOLUMECHOICE

#include <string>

#include "../util/WindowSettings.h"

namespace mp {

struct DICOMVolumeChoice
{
	//#################### PUBLIC VARIABLES ####################
	std::string dicomdirFilename, filePrefix, patientKey, studyKey, seriesKey;
	int minX, minY, minZ, maxX, maxY, maxZ;
	WindowSettings windowSettings;

	//#################### CONSTRUCTORS ####################
	DICOMVolumeChoice(const std::string& dicomdirFilename_, const std::string& patientKey_, const std::string& studyKey_, const std::string& seriesKey_, int minX_, int minY_, int minZ_, int maxX_, int maxY_, int maxZ_, const WindowSettings& windowSettings_);

	//#################### PUBLIC METHODS ####################
	std::string description() const;
};

}

#endif
