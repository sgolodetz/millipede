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
	std::string m_patientHandle, m_studyHandle, m_seriesHandle;
	int m_maxX, m_maxY, m_maxZ, m_minX, m_minY, m_minZ;
	WindowSettings m_windowSettings;

	//#################### CONSTRUCTORS ####################
public:
	// TODO
};

}

#endif
