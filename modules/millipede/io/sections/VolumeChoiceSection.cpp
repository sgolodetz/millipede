/***
 * millipede: VolumeChoiceSection.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeChoiceSection.h"

#include <boost/lexical_cast.hpp>
#include <boost/tuple/tuple.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <millipede/dicom/volumes/DICOMVolumeChoice.h>
#include <millipede/io/util/FieldIO.h>
#include <millipede/io/util/LineIO.h>

namespace mp {

//#################### LOADING METHODS ####################
DICOMVolumeChoice VolumeChoiceSection::load(std::istream& is)
try
{
	LineIO::read_checked_line(is, "VolumeChoice");
	LineIO::read_checked_line(is, "{");

	std::string dicomdirFilename, patientHandle, studyHandle, seriesHandle;
	int minX, minY, minZ, maxX, maxY, maxZ;
	WindowSettings windowSettings;

	std::string line;
	for(int count=0; ; ++count)
	{
		LineIO::read_trimmed_line(is, line, "volume choice property");
		if(line == "}")
		{
			// Note: This won't detect duplicate properties.
			const int PROPERTIES_NEEDED = 11;
			if(count == PROPERTIES_NEEDED) break;
			else throw Exception("Incorrect number of volume choice properties");
		}

		// Parse the field.
		std::string name, value;
		boost::tie(name, value) = FieldIO::parse_field(line);

		// Set the appropriate property.
		if(name == "DICOMDIR")		dicomdirFilename = value;
		else if(name == "MaxX")		maxX = lexical_cast<int>(value);
		else if(name == "MaxY")		maxY = lexical_cast<int>(value);
		else if(name == "MaxZ")		maxZ = lexical_cast<int>(value);
		else if(name == "MinX")		minX = lexical_cast<int>(value);
		else if(name == "MinY")		minY = lexical_cast<int>(value);
		else if(name == "MinZ")		minZ = lexical_cast<int>(value);
		else if(name == "Patient")	patientHandle = value;
		else if(name == "Series")	seriesHandle = value;
		else if(name == "Study")	studyHandle = value;
		else if(name == "Window")
		{
			size_t space = value.find(' ');
			double centre = lexical_cast<double>(value.substr(0, space));
			double width = lexical_cast<double>(value.substr(space+1));
			windowSettings = WindowSettings(centre, width);
		}
		else throw Exception("Unknown volume choice property: " + name);
	}

	return DICOMVolumeChoice(dicomdirFilename, patientHandle, studyHandle, seriesHandle, minX, minY, minZ, maxX, maxY, maxZ, windowSettings);
}
catch(bad_lexical_cast&)
{
	throw Exception("One of the volume choice properties was of the wrong type");
}

//#################### SAVING METHODS ####################
void VolumeChoiceSection::save(std::ostream& os, const DICOMVolumeChoice& volumeChoice)
{
	os << "VolumeChoice\n";
	os << "{\n";

	FieldIO::write_typed_field(os, "\tDICOMDIR", volumeChoice.dicomdirFilename);
	FieldIO::write_typed_field(os, "\tPatient", volumeChoice.patientKey);
	FieldIO::write_typed_field(os, "\tStudy", volumeChoice.studyKey);
	FieldIO::write_typed_field(os, "\tSeries", volumeChoice.seriesKey);
	FieldIO::write_typed_field(os, "\tMinX", volumeChoice.minX);
	FieldIO::write_typed_field(os, "\tMinY", volumeChoice.minY);
	FieldIO::write_typed_field(os, "\tMinZ", volumeChoice.minZ);
	FieldIO::write_typed_field(os, "\tMaxX", volumeChoice.maxX);
	FieldIO::write_typed_field(os, "\tMaxY", volumeChoice.maxY);
	FieldIO::write_typed_field(os, "\tMaxZ", volumeChoice.maxZ);
	os << "\tWindow = " << volumeChoice.windowSettings.centre() << ' ' << volumeChoice.windowSettings.width() << '\n';

	os << "}\n";
}

}
