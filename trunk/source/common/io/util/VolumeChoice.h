/***
 * millipede: VolumeChoice.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMECHOICE
#define H_MILLIPEDE_VOLUMECHOICE

#include <string>

#include <common/dicom/util/WindowSettings.h>

namespace mp {

struct VolumeChoice
{
	//#################### PUBLIC VARIABLES ####################
	std::string filePrefix, patientKey, studyKey, seriesKey;
	int minX, minY, minZ, maxX, maxY, maxZ;
	WindowSettings windowSettings;

	//#################### CONSTRUCTORS ####################
	VolumeChoice(const std::string& filePrefix_, const std::string& patientKey_, const std::string& studyKey_, const std::string& seriesKey_, int minX_, int minY_, int minZ_, int maxX_, int maxY_, int maxZ_, const WindowSettings& windowSettings_);
};

}

#endif
