/***
 * millipede: VolumeChoice.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeChoice.h"

namespace mp {

//#################### CONSTRUCTORS ####################
VolumeChoice::VolumeChoice(const std::string& filePrefix, const std::string& patientHandle, const std::string& studyHandle, const std::string& seriesHandle,
						   int minX, int minY, int minZ, int maxX, int maxY, int maxZ, const WindowSettings& windowSettings)
:	m_filePrefix(filePrefix), m_patientHandle(patientHandle), m_studyHandle(studyHandle), m_seriesHandle(seriesHandle),
	m_minX(minX), m_minY(minY), m_minZ(minZ), m_maxX(maxX), m_maxY(maxY), m_maxZ(maxZ), m_windowSettings(windowSettings)
{}

}
