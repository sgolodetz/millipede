/***
 * test-rootedmst: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>

#include <common/adts/RootedMST.h>
#include <common/partitionforests/images/ImageLeafLayer.h>
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
	RootedMST<int, int> mst(graph);
}

void itk_leaf_layer_mst()
{
	typedef itk::Image<signed int,3> HounsfieldImage;
	typedef HounsfieldImage::Pointer HounsfieldImagePointer;
	typedef itk::Image<unsigned char,3> WindowedImage;
	typedef WindowedImage::Pointer WindowedImagePointer;

	signed int hounsfieldPixels[] = {
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

	ImageLeafLayer leafLayer(hounsfieldImage, windowedImage);

	typedef int Label;
	RootedMST<Label,ImageLeafLayer::EdgeWeight> mst(leafLayer);
}

void leaf_layer_mst()
{
	typedef PixelProperties P;
	PixelProperties arr[] = { P(0,0), P(1,1), P(2,2), P(3,3), P(4,4), P(5,5), P(6,6), P(7,7), P(8,8) };
	std::vector<PixelProperties> leafProperties(&arr[0], &arr[sizeof(arr)/sizeof(PixelProperties)]);
	ImageLeafLayer leafLayer(3, 3, 1, leafProperties);

	typedef int Label;
	typedef RootedMST<Label,ImageLeafLayer::EdgeWeight> MST;
	MST mst(leafLayer);

	mst.merge_nodes(7, 6);

	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<MST::Edge>(std::cout, " "));
}

int main()
{
	adjacency_graph_mst();
	itk_leaf_layer_mst();
	leaf_layer_mst();
	return 0;
}
