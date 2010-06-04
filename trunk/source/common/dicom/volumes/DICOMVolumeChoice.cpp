/***
 * millipede: DICOMVolumeChoice.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "DICOMVolumeChoice.h"

#include <sstream>

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMVolumeChoice::DICOMVolumeChoice(const std::string& dicomdirFilename_, const std::string& patientKey_, const std::string& studyKey_, const std::string& seriesKey_,
									 int minX_, int minY_, int minZ_, int maxX_, int maxY_, int maxZ_, const WindowSettings& windowSettings_)
:	dicomdirFilename(dicomdirFilename_), patientKey(patientKey_), studyKey(studyKey_), seriesKey(seriesKey_),
	minX(minX_), minY(minY_), minZ(minZ_), maxX(maxX_), maxY(maxY_), maxZ(maxZ_), windowSettings(windowSettings_)
{
	filePrefix = dicomdirFilename.substr(0, dicomdirFilename.length()-8);		// the prefix is the DICOMDIR path without 'DICOMDIR' on the end of it
}

//#################### PUBLIC METHODS ####################
std::string DICOMVolumeChoice::description() const
{
	std::ostringstream oss;
	oss << patientKey << " | " << studyKey << " | " << seriesKey;
	return oss.str();
}

}
