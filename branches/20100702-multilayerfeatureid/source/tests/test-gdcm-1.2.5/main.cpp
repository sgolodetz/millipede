/***
 * test-gdcm-1.2.5: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <cstdlib>
#include <iostream>

#include <gdcmDicomDir.h>
#include <gdcmDicomDirImage.h>
#include <gdcmDicomDirPatient.h>
#include <gdcmDicomDirSerie.h>
#include <gdcmDicomDirStudy.h>

int main()
{
	// Load in the example DICOMDIR.
	gdcm::DicomDir dicomdir;
	dicomdir.SetFileName("../resources/DICOMDIR");
	if(dicomdir.Load())
	{
		std::cout << "Successfully loaded DICOMDIR\n";
	}
	else return EXIT_FAILURE;

	// Output the patients.
	for(gdcm::DicomDirPatient *patient=dicomdir.GetFirstPatient(); patient!=NULL; patient=dicomdir.GetNextPatient())
	{
		// Output the patient's name.
		std::cout << patient->GetEntryValue(0x0010, 0x0010) << '\n';

		// Output the studies.
		for(gdcm::DicomDirStudy *study=patient->GetFirstStudy(); study!=NULL; study=patient->GetNextStudy())
		{
			// Output the study's description.
			std::cout << '\t' << study->GetEntryValue(0x0008, 0x1030) << '\n';

			// Output the series.
			for(gdcm::DicomDirSerie *serie=study->GetFirstSerie(); serie!=NULL; serie=study->GetNextSerie())
			{
				// Output the serie's number and image count.
				std::cout << "\t\t" << serie->GetEntryValue(0x0020, 0x0011) << ": ";

				int imageCount = 0;
				for(gdcm::DicomDirImage *image=serie->GetFirstImage(); image!=NULL; image=serie->GetNextImage())
				{
					++imageCount;
				}

				std::cout << imageCount << " image(s)\n";
			}
		}
	}

	return 0;
}
