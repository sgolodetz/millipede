/***
 * millipede: DICOMDIRFile.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "io/files/DICOMDIRFile.h"

#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/trim.hpp>

#include <gdcm-1.2/gdcmDicomDir.h>
#include <gdcm-1.2/gdcmDicomDirImage.h>
#include <gdcm-1.2/gdcmDicomDirPatient.h>
#include <gdcm-1.2/gdcmDicomDirSerie.h>
#include <gdcm-1.2/gdcmDicomDirStudy.h>

#include "dicom/directories/PatientRecord.h"
#include "dicom/directories/SeriesRecord.h"
#include "dicom/directories/StudyRecord.h"
#include "exceptions/Exception.h"

namespace mp {

//#################### LOADING METHODS ####################
DICOMDirectory_Ptr DICOMDIRFile::load(const std::string& filename)
{
	// Note: The hex values in this function are DICOM tags - they can be looked up in the DICOM standard.

	DICOMDirectory_Ptr ret(new DICOMDirectory);

	// Load in the DICOMDIR.
	gdcm1::DicomDir dicomdir;
	dicomdir.SetFileName(filename);

	// MEMORY LEAK: The gdcm library leaks memory here (there's nothing obvious I can do about it besides document it unfortunately).
	if(!dicomdir.Load())
	{
		throw Exception("Failed to load DICOM directory from: " + filename);
	}

	// Add the patients to the directory.
	for(gdcm1::DicomDirPatient *patient=dicomdir.GetFirstPatient(); patient!=NULL; patient=dicomdir.GetNextPatient())
	{
		std::string patientsName = patient->GetEntryValue(0x0010, 0x0010);
		PatientRecord_Ptr patientRecord(new PatientRecord(patientsName));

		// Add the studies to the patient record.
		for(gdcm1::DicomDirStudy *study=patient->GetFirstStudy(); study!=NULL; study=patient->GetNextStudy())
		{
			std::string studyDescription = study->GetEntryValue(0x0008, 0x1030);
			std::string studyID = study->GetEntryValue(0x0020, 0x0010);
			std::string studyInstanceUID = study->GetEntryValue(0x0020, 0x000d);

			// Sanitize values (bleurgh - this really shouldn't be necessary).
			studyInstanceUID = studyInstanceUID.substr(0, studyInstanceUID.find_first_of('\0'));

			StudyRecord_Ptr studyRecord(new StudyRecord(studyDescription, studyID, studyInstanceUID));

			// Add the series to the study record.
			for(gdcm1::DicomDirSerie *serie=study->GetFirstSerie(); serie!=NULL; serie=study->GetNextSerie())
			{
				std::string seriesNumber = serie->GetEntryValue(0x0020, 0x0011);
				boost::trim(seriesNumber);
				SeriesRecord_Ptr seriesRecord(new SeriesRecord(seriesNumber));

				// Add the image filenames to the series record.
				for(gdcm1::DicomDirImage *image=serie->GetFirstImage(); image!=NULL; image=serie->GetNextImage())
				{
					std::string referencedFileID = image->GetEntryValue(0x0004, 0x1500);
					boost::replace_all(referencedFileID, "\\", "/");
					boost::trim(referencedFileID);
					seriesRecord->add_image_filename(referencedFileID);
				}

				studyRecord->add_series_record(seriesRecord);
			}

			patientRecord->add_study_record(studyRecord);
		}

		ret->add_patient_record(patientRecord);
	}

	return ret;
}

}