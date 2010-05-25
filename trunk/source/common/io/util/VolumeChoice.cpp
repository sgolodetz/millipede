/***
 * millipede: VolumeChoice.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeChoice.h"

#include <sstream>

namespace mp {

//#################### CONSTRUCTORS ####################
VolumeChoice::VolumeChoice(const std::string& filePrefix_, const std::string& patientKey_, const std::string& studyKey_, const std::string& seriesKey_,
						   int minX_, int minY_, int minZ_, int maxX_, int maxY_, int maxZ_, const WindowSettings& windowSettings_)
:	filePrefix(filePrefix_), patientKey(patientKey_), studyKey(studyKey_), seriesKey(seriesKey_),
	minX(minX_), minY(minY_), minZ(minZ_), maxX(maxX_), maxY(maxY_), maxZ(maxZ_), windowSettings(windowSettings_)
{}

//#################### PUBLIC METHODS ####################
std::string VolumeChoice::description() const
{
	std::ostringstream oss;
	oss << patientKey << " | " << studyKey << " | " << seriesKey;
	return oss.str();
}

}
