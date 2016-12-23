/***
 * millipede: DICOMDIRFile.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMDIRFILE
#define H_MILLIPEDE_DICOMDIRFILE

#include <string>

#include "../../dicom/directories/DICOMDirectory.h"

namespace mp {

struct DICOMDIRFile
{
	//#################### LOADING METHODS ####################
	static DICOMDirectory_Ptr load(const std::string& filename);
};

}

#endif
