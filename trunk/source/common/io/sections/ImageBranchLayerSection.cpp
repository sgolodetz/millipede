/***
 * millipede: ImageBranchLayerSection.cpp
 * Added by Varduhi Yeghiazaryan, 2013.
 ***/

#include "ImageBranchLayerSection.h"

#include <boost/lexical_cast.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <common/io/util/LineIO.h>

namespace mp {

//#################### LOADING METHODS ####################
ImageBranchLayerSection::ImageBranchLayer_Ptr ImageBranchLayerSection::load(std::istream& is)
try
{
	ImageBranchLayer_Ptr imageBranchLayer(new ImageBranchLayer());
	
	LineIO::read_checked_line(is, "ImageBranchLayer");
	LineIO::read_checked_line(is, "{");

	std::string line;
	int node;
	for(;;)
	{
		LineIO::read_line(is, line, "image branch layer node index");
		if(line == "|")	break;
		
		node = lexical_cast<int>(line);
		
		LineIO::read_line(is, line, "image branch layer node properties");
		imageBranchLayer->set_node_properties(node, lexical_cast<NodeProperties>(line));

		std::set<int> nodeChildren;

		LineIO::read_checked_line(is, "{");
		for(;;)
		{
			LineIO::read_line(is, line, "image branch layer node child");
			if(line == "}")	break;
			
			nodeChildren.insert(lexical_cast<int>(line));
		}
		imageBranchLayer->set_node_children(node, nodeChildren);		

		LineIO::read_line(is, line, "image branch layer node parent");
		imageBranchLayer->set_node_parent(node, lexical_cast<int>(line));
	}

	for(;;)
	{
		LineIO::read_line(is, line, "image branch layer edge");
		if(line == "}")	break;
	
		size_t space = line.find(' ');
		size_t rightBrace = line.find('}');
		int u = lexical_cast<int>(line.substr(2, space-2));
		int v = lexical_cast<int>(line.substr(space+1, rightBrace-space-1));
		ImageBranchLayer::EdgeWeight w = lexical_cast<ImageBranchLayer::EdgeWeight>(line.substr(rightBrace+3, line.size()-1-rightBrace-3));
		
		imageBranchLayer->set_edge_weight(u, v, w);		
	}

	return imageBranchLayer;	
}
catch(bad_lexical_cast&)
{
	throw Exception("One of the image branch layer properties was of the wrong type");
}

//#################### SAVING METHODS ####################
void ImageBranchLayerSection::save(std::ostream& os, const ImageBranchLayer_Ptr& imageBranchLayer)
{
	os << "ImageBranchLayer\n";
	os << "{\n";
	
	std::vector<int> nodeIndices = imageBranchLayer->node_indices();
	for(int i=0, size=static_cast<int>(nodeIndices.size()); i<size; ++i)
	{
		os	<< nodeIndices[i] << '\n'
			<< imageBranchLayer->node_properties(nodeIndices[i]) << '\n'
			<< "{\n";

		std::set<int> children = imageBranchLayer->node_children(nodeIndices[i]);
		std::copy(children.begin(), children.end(), std::ostream_iterator<int>(os, "\n"));

		os	<< "}\n"
			<< imageBranchLayer->node_parent(nodeIndices[i]) << '\n';
	}

	os << "|\n";
	
	std::vector<ImageBranchLayer::Edge> edges = imageBranchLayer->edges();
	for(int i=0, size=static_cast<int>(edges.size()); i<size; ++i)
	{
		os << edges[i] << '\n';
	}

	os << "}\n";
}

}
