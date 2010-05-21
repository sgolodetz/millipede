/***
 * test-rootedmst: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>

#include <common/adts/RootedMST.h>
#include <common/partitionforests/images/CTImageLeafLayer.h>
#include <common/partitionforests/images/SimpleImageLeafLayer.h>
using namespace mp;

template <typename TPixel>
typename itk::Image<TPixel,3>::Pointer create_3d_image(const TPixel *const pixels, int sizeX, int sizeY, int sizeZ)
{
	typedef itk::Image<TPixel,3> Image;
	typedef typename Image::Pointer ImagePointer;

	typename Image::IndexType start;
	start.Fill(0);
	typename Image::SizeType size;
	size[0] = sizeX;
	size[1] = sizeY;
	size[2] = sizeZ;
	typename Image::RegionType region;
	region.SetIndex(start);
	region.SetSize(size);
	ImagePointer image = Image::New();
	image->SetRegions(region);
	image->Allocate();

	const TPixel *p = pixels;
	for(int z=0; z<sizeZ; ++z)
		for(int y=0; y<sizeY; ++y)
			for(int x=0; x<sizeX; ++x)
			{
				typename Image::IndexType index;
				index[0] = x;
				index[1] = y;
				index[2] = z;
				image->SetPixel(index, *p++);
			}

	return image;
}

void adjacency_graph_mst()
{
	AdjacencyGraph<std::string, int> graph;
	graph.set_node_properties(0, "Wibble");
	RootedMST<int> mst(graph);
}

void ct_leaf_layer_mst()
{
	typedef itk::Image<int,3>::Pointer GradientMagnitudeImagePointer;
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

	HounsfieldImagePointer hounsfieldImage = create_3d_image(hounsfieldPixels, 3, 3, 2);
	WindowedImagePointer windowedImage = create_3d_image(windowedPixels, 3, 3, 2);
	GradientMagnitudeImagePointer gradientMagnitudeImage = hounsfieldImage;		// note: this is a hack (for testing purposes)

	CTImageLeafLayer leafLayer(hounsfieldImage, windowedImage, gradientMagnitudeImage);

	RootedMST<CTImageLeafLayer::EdgeWeight> mst(leafLayer);
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
