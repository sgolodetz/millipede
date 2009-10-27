/***
 * millipede: VolumeChoice.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMECHOICE
#define H_MILLIPEDE_VOLUMECHOICE

#include <string>

#include <common/dicom/util/WindowSettings.h>

namespace mp {

class VolumeChoice
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_filePrefix, m_patientHandle, m_studyHandle, m_seriesHandle;
	int m_minX, m_minY, m_minZ, m_maxX, m_maxY, m_maxZ;
	WindowSettings m_windowSettings;

	//#################### CONSTRUCTORS ####################
public:
	VolumeChoice(const std::string& filePrefix, const std::string& patientHandle, const std::string& studyHandle, const std::string& seriesHandle, int minX, int minY, int minZ, int maxX, int maxY, int maxZ, const WindowSettings& windowSettings);
};

}

#endif
