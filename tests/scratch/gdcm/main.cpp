/***
 * scratchtest_gdcm: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <cstdlib>
#include <iostream>

#include <gdcm-1.2/gdcmDicomDir.h>
#include <gdcm-1.2/gdcmDicomDirImage.h>
#include <gdcm-1.2/gdcmDicomDirPatient.h>
#include <gdcm-1.2/gdcmDicomDirSerie.h>
#include <gdcm-1.2/gdcmDicomDirStudy.h>

#include <srgutil/filesystem/PathFinder.h>
using namespace srgutil;

namespace bf = boost::filesystem;

int main()
{
	const bf::path resourcesDir = find_subdir_from_executable("resources");

	// Load in the example DICOMDIR.
	gdcm1::DicomDir dicomdir;
	dicomdir.SetFileName((resourcesDir / "DICOMDIR").string());
	if(dicomdir.Load())
	{
		std::cout << "Successfully loaded DICOMDIR\n";
	}
	else return EXIT_FAILURE;

	// Output the patients.
	for(gdcm1::DicomDirPatient *patient=dicomdir.GetFirstPatient(); patient!=NULL; patient=dicomdir.GetNextPatient())
	{
		// Output the patient's name.
		std::cout << patient->GetEntryValue(0x0010, 0x0010) << '\n';

		// Output the studies.
		for(gdcm1::DicomDirStudy *study=patient->GetFirstStudy(); study!=NULL; study=patient->GetNextStudy())
		{
			// Output the study's description.
			std::cout << '\t' << study->GetEntryValue(0x0008, 0x1030) << '\n';

			// Output the series.
			for(gdcm1::DicomDirSerie *serie=study->GetFirstSerie(); serie!=NULL; serie=study->GetNextSerie())
			{
				// Output the serie's number and image count.
				std::cout << "\t\t" << serie->GetEntryValue(0x0020, 0x0011) << ": ";

				int imageCount = 0;
				for(gdcm1::DicomDirImage *image=serie->GetFirstImage(); image!=NULL; image=serie->GetNextImage())
				{
					++imageCount;
				}

				std::cout << imageCount << " image(s)\n";
			}
		}
	}

	return 0;
}
