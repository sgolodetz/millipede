/***
 * millipede: ImageLeafLayerSection.cpp
 * Added by Varduhi Yeghiazaryan, 2013.
 ***/

#include "ImageLeafLayerSection.h"

#include <boost/lexical_cast.hpp>
#include <boost/tuple/tuple.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <common/io/util/FieldIO.h>
#include <common/io/util/LineIO.h>

namespace mp {

//#################### LOADING METHODS ####################
ImageLeafLayerSection::ImageLeafLayer_Ptr ImageLeafLayerSection::load(std::istream& is)
try
{
	LineIO::read_checked_line(is, "ImageLeafLayer");
	LineIO::read_checked_line(is, "{");

	int sizeX, sizeY, sizeZ;
	std::vector<NodeProperties> nodeProperties;
	std::vector<int> nodeParents; 
	int nodesCount;

	std::string line;
	for(int count=0; ; ++count)
	{
		LineIO::read_trimmed_line(is, line, "image leaf layer property");
		if(line == "|")
		{
			// Note: This won't detect duplicate properties.
			const int PROPERTIES_NEEDED = 3;
			if(count == PROPERTIES_NEEDED) break;
			else throw Exception("Incorrect number of image leaf layer properties");
		}

		// Parse the field.
		std::string name, value;
		boost::tie(name, value) = FieldIO::parse_field(line);

		// Set the appropriate property.
		if(name == "SizeX")		sizeX = lexical_cast<int>(value);
		else if(name == "SizeY")	sizeY = lexical_cast<int>(value);
		else if(name == "SizeZ")	sizeZ = lexical_cast<int>(value);
		else throw Exception("Unknown image leaf layer property: " + name);
	}

	for(nodesCount=0; ; ++nodesCount)
	{
		LineIO::read_line(is, line, "image leaf layer node properties");
		if(line == "}")	break;	
		
		nodeProperties.push_back(lexical_cast<NodeProperties>(line));
		
		LineIO::read_line(is, line, "image leaf layer node parent");
		nodeParents.push_back(lexical_cast<int>(line));		
	}

	ImageLeafLayer_Ptr imageLeafLayer(new ImageLeafLayer(nodeProperties, sizeX, sizeY, sizeZ));

	for(int i=0; i<nodesCount; ++i)
	{
		imageLeafLayer->set_node_parent(i, nodeParents[i]);
	}

	return imageLeafLayer;
}
catch(bad_lexical_cast&)
{
	throw Exception("One of the image leaf layer properties was of the wrong type");
}

//#################### SAVING METHODS ####################
void ImageLeafLayerSection::save(std::ostream& os, const ImageLeafLayer_Ptr& imageLeafLayer)
{
	os << "ImageLeafLayer\n";
	os << "{\n";

	FieldIO::write_typed_field(os, "\tSizeX", imageLeafLayer->size_x());
	FieldIO::write_typed_field(os, "\tSizeY", imageLeafLayer->size_y());
	FieldIO::write_typed_field(os, "\tSizeZ", imageLeafLayer->size_z());
	
	os << "|\n";

	for(int i=0, size=imageLeafLayer->node_count(); i<size; ++i)
	{
		os	<< imageLeafLayer->node_properties(i) << '\n'
			<< imageLeafLayer->node_parent(i) << '\n';	
	}
	
	os << "}\n";
}

}
