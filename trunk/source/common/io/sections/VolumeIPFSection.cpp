/***
 * millipede: VolumeIPFSection.cpp
 * Added by Varduhi Yeghiazaryan, 2013.
 ***/

#include "VolumeIPFSection.h"

#include <boost/lexical_cast.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <common/io/sections/ImageBranchLayerSection.h>
#include <common/io/sections/ImageLeafLayerSection.h>
#include <common/io/util/LineIO.h>

namespace mp {

//#################### LOADING METHODS ####################
VolumeIPFSection::VolumeIPF_Ptr VolumeIPFSection::load(std::istream& is)
try
{
	LineIO::read_checked_line(is, "VolumeIPF");
	LineIO::read_checked_line(is, "{");
	
	std::string line;
	LineIO::read_line(is, line, "volume size");

	size_t firstComma = line.find(',');
	size_t secondComma = line.find(',', firstComma+1);
	int firstElement = lexical_cast<int>(line.substr(1, firstComma-1));
	int secondElement = lexical_cast<int>(line.substr(firstComma+2, secondComma-firstComma-2));
	int thirdElement = lexical_cast<int>(line.substr(secondComma+2, line.size()-1-secondComma-2));

	itk::Size<3> volumeSize = {{firstElement, secondElement, thirdElement}};

	LeafLayer_Ptr imageLeafLayer = ImageLeafLayerSection::load(is);
	
	VolumeIPF_Ptr volumeIPF(new VolumeIPFT(volumeSize, imageLeafLayer));
	
	LineIO::read_line(is, line, "number of branch layers");
	int highestLayer = lexical_cast<int>(line);
	
	for(int layer=1; layer<=highestLayer; ++layer)
	{
		BranchLayer_Ptr imageBranchLayer = ImageBranchLayerSection::load(is);
		volumeIPF->add_branch_layer(imageBranchLayer);
	}
	
	LineIO::read_checked_line(is, "}");	
	
	return volumeIPF;
}
catch(bad_lexical_cast&)
{
	throw Exception("One of the volume IPF properties was of the wrong type");
}

//#################### SAVING METHODS ####################
void VolumeIPFSection::save(std::ostream& os, const VolumeIPF_Ptr& volumeIPF)
{
	os << "VolumeIPF\n";
	os << "{\n";
	
	os << volumeIPF->volume_size() << '\n';
	
	ImageLeafLayerSection::save(os, volumeIPF->leaf_layer());
	
	int highestLayer = volumeIPF->highest_layer();
	os << highestLayer << '\n';

	for(int layer=1; layer<=highestLayer; ++layer)
	{
		ImageBranchLayerSection::save(os, volumeIPF->branch_layer(layer));
	}
	
	os << "}\n";
}	

}

