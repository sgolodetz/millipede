/***
 * millipede: VolumeChoiceSection.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeChoiceSection.h"

#include <boost/lexical_cast.hpp>
#include <boost/tuple/tuple.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <common/io/util/FieldIO.h>
#include <common/io/util/LineIO.h>

namespace mp {

//#################### LOADING METHODS ####################
VolumeChoice VolumeChoiceSection::load(std::istream& is)
try
{
	LineIO::read_checked_line(is, "VolumeChoice");
	LineIO::read_checked_line(is, "{");

	std::string filePrefix, patientHandle, studyHandle, seriesHandle;
	int maxX, maxY, maxZ, minX, minY, minZ;
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
		if(name == "MaxX")			maxX = lexical_cast<int>(value);
		else if(name == "MaxY")		maxY = lexical_cast<int>(value);
		else if(name == "MaxZ")		maxZ = lexical_cast<int>(value);
		else if(name == "MinX")		minX = lexical_cast<int>(value);
		else if(name == "MinY")		minY = lexical_cast<int>(value);
		else if(name == "MinZ")		minZ = lexical_cast<int>(value);
		else if(name == "Patient")	patientHandle = value;
		else if(name == "Prefix")	filePrefix = value;
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

	return VolumeChoice(filePrefix, patientHandle, studyHandle, seriesHandle, minX, minY, minZ, maxX, maxY, maxZ, windowSettings);
}
catch(bad_lexical_cast&)
{
	throw Exception("One of the volume choice properties was of the wrong type");
}

}
