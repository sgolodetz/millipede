/***
 * test-rootedmst: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>

#include <common/adts/RootedMST.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/SimpleImageLeafLayer.h>
#include <common/util/ITKImageUtil.h>
using namespace mp;

void adjacency_graph_mst()
{
	AdjacencyGraph<std::string, int> graph;
	graph.set_node_properties(0, "Wibble");
	RootedMST<int> mst(graph);
}

void ct_leaf_layer_mst()
{
	typedef itk::Image<short,3>::Pointer GradientMagnitudeImagePointer;
	typedef itk::Image<int,3>::Pointer HounsfieldImagePointer;
	typedef itk::Image<unsigned char,3>::Pointer WindowedImagePointer;

	int hounsfieldPixels[] = {
		-3, -3, -1,
		2, 1, 0,
		5, 4, 2,

		10, 9, 8,
		8, 11, 7,
		6, 7, 12
	};

	unsigned char windowedPixels[] = {
		0, 0, 2,
		5, 4, 3,
		8, 7, 5,

		13, 12, 11,
		11, 14, 10,
		9, 10, 15
	};

	// Note: These aren't intended to be the correct values, they're just for testing purposes.
	short gradientMagnitudePixels[] = {
		-3, -3, -1,
		2, 1, 0,
		5, 4, 2,

		10, 9, 8,
		8, 11, 7,
		6, 7, 12
	};

	HounsfieldImagePointer hounsfieldImage = ITKImageUtil::make_filled_image(3, 3, 2, hounsfieldPixels);
	WindowedImagePointer windowedImage = ITKImageUtil::make_filled_image(3, 3, 2, windowedPixels);
	GradientMagnitudeImagePointer gradientMagnitudeImage = ITKImageUtil::make_filled_image(3, 3, 2, gradientMagnitudePixels);

	DICOMImageLeafLayer leafLayer(hounsfieldImage, windowedImage, gradientMagnitudeImage);

	RootedMST<DICOMImageLeafLayer::EdgeWeight> mst(leafLayer);
}

void simple_leaf_layer_mst()
{
	SimplePixelProperties arr[] = {0,1,2,3,4,5,6,7,8};
	std::vector<SimplePixelProperties> leafProperties(&arr[0], &arr[sizeof(arr)/sizeof(SimplePixelProperties)]);
	SimpleImageLeafLayer leafLayer(leafProperties, 3, 3);

	typedef RootedMST<SimpleImageLeafLayer::EdgeWeight> MST;
	MST mst(leafLayer);

	mst.merge_nodes(7, 6);

	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<MST::Edge>(std::cout, " "));
}

int main()
{
	adjacency_graph_mst();
	ct_leaf_layer_mst();
	simple_leaf_layer_mst();
	return 0;
}
