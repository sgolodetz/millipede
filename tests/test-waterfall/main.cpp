/***
 * test-waterfall: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>

#include <boost/algorithm/string/replace.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <itkCastImageFilter.h>
#include <itkGradientAnisotropicDiffusionImageFilter.h>
#include <itkGradientMagnitudeImageFilter.h>
#include <itkImageFileReader.h>
#include <itkImageFileWriter.h>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/partitionforests/base/PartitionForest.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/segmentation/waterfall/GolodetzWaterfallPass.h>
#include <common/segmentation/waterfall/MarcoteguiWaterfallPass.h>
#include <common/segmentation/waterfall/NichollsWaterfallPass.h>
#include <common/segmentation/watershed/MeijsterRoerdinkWatershed.h>
#include <common/util/ITKImageUtil.h>
using namespace mp;

typedef PartitionForest<DICOMImageLeafLayer,DICOMImageBranchLayer> IPF;
typedef shared_ptr<IPF> IPF_Ptr;

//#################### HELPER FUNCTIONS ####################
itk::Image<unsigned char,2>::Pointer make_mosaic_image(const boost::shared_ptr<const PartitionForest<DICOMImageLeafLayer,DICOMImageBranchLayer> >& ipf,
													   int layerIndex, int width, int height)
{
	typedef itk::Image<unsigned char,2> Image;
	typedef PartitionForest<DICOMImageLeafLayer,DICOMImageBranchLayer> IPF;

	Image::Pointer image = ITKImageUtil::make_image<unsigned char>(width, height);

	Image::IndexType index;
	int n = 0;
	for(index[1]=0; index[1]<height; ++index[1])
		for(index[0]=0; index[0]<width; ++index[0])
		{
			unsigned char mosaicValue;
			if(layerIndex > 0)
			{
				PFNodeID ancestor = ipf->ancestor_of(PFNodeID(0, n), layerIndex);
				mosaicValue = static_cast<unsigned char>(ipf->branch_properties(ancestor).mean_grey_value());
			}
			else mosaicValue = ipf->leaf_properties(n).grey_value();

			image->SetPixel(index, mosaicValue);
			++n;
		}

	return image;
}

itk::Image<unsigned char,2>::Pointer
make_mosaic_image_with_boundaries(const boost::shared_ptr<const PartitionForest<DICOMImageLeafLayer,DICOMImageBranchLayer> >& ipf, int layerIndex,
								  int width, int height)
{
	typedef itk::Image<PFNodeID,2> AncestorImage;
	typedef itk::Image<unsigned char,2> MosaicImage;
	typedef PartitionForest<DICOMImageLeafLayer,DICOMImageBranchLayer> IPF;

	// Create an image of the ancestors of the pixels in the specified layer.
	AncestorImage::Pointer ancestorImage = ITKImageUtil::make_image<PFNodeID>(width, height);

	AncestorImage::IndexType ancestorIndex;
	int n = 0;
	for(ancestorIndex[1]=0; ancestorIndex[1]<height; ++ancestorIndex[1])
		for(ancestorIndex[0]=0; ancestorIndex[0]<width; ++ancestorIndex[0])
		{
			ancestorImage->SetPixel(ancestorIndex, ipf->ancestor_of(PFNodeID(0, n), layerIndex));
			++n;
		}

	// Set up an iterator to traverse the ancestor image, whilst allowing us to access the neighbours of each pixel.
	typedef itk::ConstShapedNeighborhoodIterator<AncestorImage> ConstShapedNeighbourhoodIteratorType;
	AncestorImage::SizeType radius = {{1,1}};
	ConstShapedNeighbourhoodIteratorType it(radius, ancestorImage, ancestorImage->GetLargestPossibleRegion());
	std::vector<AncestorImage::OffsetType> offsets = ITKImageUtil::make_4_connected_offsets();
	for(size_t k=0, size=offsets.size(); k<size; ++k)
	{
		it.ActivateOffset(offsets[k]);
	}

	// Set up a boundary condition that makes pixels beyond the boundary equal to those on it. This is the
	// right boundary condition here, because the idea is to mark pixels as boundaries when they have an
	// adjacent neighbour with a different ancestor. We don't want there to be spurious boundaries on the
	// borders of the image, so we need to make sure that the pixels beyond the image have the same ancestors
	// as their respective neighbours within it.
	itk::ZeroFluxNeumannBoundaryCondition<AncestorImage> condition;
	it.OverrideBoundaryCondition(&condition);

	// Create the mosaic image by traversing the ancestor image. We mark boundaries where appropriate, and
	// obtain the non-boundary mosaic values from the properties of the ancestor nodes.
	MosaicImage::Pointer mosaicImage = ITKImageUtil::make_image<unsigned char>(width, height);

	for(it.GoToBegin(); !it.IsAtEnd(); ++it)
	{
		bool regionBoundary = false;
		for(ConstShapedNeighbourhoodIteratorType::ConstIterator jt=it.Begin(), jend=it.End(); jt!=jend; ++jt)
		{
			if(jt.Get() != it.GetCenterPixel())
			{
				regionBoundary = true;
				break;
			}
		}

#if 1
		// Version which fills the mosaic regions with their mean grey values.
		unsigned char mosaicValue;
		if(regionBoundary)		mosaicValue = std::numeric_limits<unsigned char>::max();
		else if(layerIndex > 0)	mosaicValue = static_cast<unsigned char>(ipf->branch_properties(it.GetCenterPixel()).mean_grey_value());
		else					mosaicValue = ipf->leaf_properties(it.GetCenterPixel().index()).grey_value();
		mosaicImage->SetPixel(it.GetIndex(), mosaicValue);
#else
		// Version which fills the mosaic regions with black (thereby just showing the mosaic borders).
		if(regionBoundary)	mosaicImage->SetPixel(it.GetIndex(), std::numeric_limits<unsigned char>::max());
		else				mosaicImage->SetPixel(it.GetIndex(), 0);
#endif
	}

	return mosaicImage;
}

void output_mosaic_image(const IPF_Ptr& ipf, int layerIndex, int width, int height, std::string outputSpecifier)
{
	typedef itk::Image<unsigned char,2> Image;
	Image::Pointer image = layerIndex > 0 ?
		make_mosaic_image_with_boundaries(ipf, layerIndex, width, height) :
		make_mosaic_image(ipf, layerIndex, width, height);

	typedef itk::ImageFileWriter<Image> Writer;
	Writer::Pointer writer = Writer::New();
	writer->SetInput(image);
	boost::replace_all(outputSpecifier, "*", boost::lexical_cast<std::string>(layerIndex));
	writer->SetFileName("../resources/" + outputSpecifier);
	writer->Update();
}

//#################### WATERFALL PASS LISTENERS ####################
struct BasicListener : WaterfallPass<int>::Listener
{
	void merge_nodes(int u, int v)
	{
		std::cout << "Merging nodes " << u << " and " << v << '\n';
	}
};

struct IPFConstructionListener : WaterfallPass<int>::Listener
{
	IPF_Ptr m_ipf;

	explicit IPFConstructionListener(const IPF_Ptr& ipf)
	:	m_ipf(ipf)
	{}

	void merge_nodes(int u, int v)
	{
		// Merge the corresponding nodes in the top-most layer of the IPF.
		std::set<PFNodeID> mergees;
		mergees.insert(PFNodeID(m_ipf->highest_layer(), u));
		mergees.insert(PFNodeID(m_ipf->highest_layer(), v));
		m_ipf->merge_sibling_nodes(mergees, IPF::DONT_CHECK_PRECONDITIONS);
	}
};

//#################### TEST FUNCTIONS ####################
void basic_test()
{
	// Create the graph in the Marcotegui waterfall paper.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<14; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 3);
		graph.set_edge_weight(1, 2, 2);
		graph.set_edge_weight(1, 3, 20);
			graph.set_edge_weight(3, 4, 4);
				graph.set_edge_weight(4, 5, 2);
			graph.set_edge_weight(3, 6, 10);
				graph.set_edge_weight(6, 7, 5);
					graph.set_edge_weight(7, 8, 5);
						graph.set_edge_weight(8, 9, 4);
						graph.set_edge_weight(8, 10, 2);
						graph.set_edge_weight(8, 11, 20);
							graph.set_edge_weight(11, 12, 4);
							graph.set_edge_weight(11, 13, 4);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Nicholls waterfall pass on the MST.
	NichollsWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void comparison_test()
{
	// Create the graph.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<9; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 1);
		graph.set_edge_weight(1, 2, 2);
			graph.set_edge_weight(2, 3, 2);
				graph.set_edge_weight(3, 6, 1);
			graph.set_edge_weight(2, 4, 2);
				graph.set_edge_weight(4, 7, 1);
			graph.set_edge_weight(2, 5, 2);
				graph.set_edge_weight(5, 8, 3);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a waterfall pass on the MST.
	GolodetzWaterfallPass<int> pass;
	//MarcoteguiWaterfallPass<int> pass;
	//NichollsWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void golodetz_test_A()
{
	// Create the graph.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<8; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 4);
		graph.set_edge_weight(1, 2, 4);
			graph.set_edge_weight(2, 4, 4);
				graph.set_edge_weight(4, 6, 1);
			graph.set_edge_weight(2, 5, 4);
				graph.set_edge_weight(5, 7, 1);
		graph.set_edge_weight(1, 3, 4);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Golodetz waterfall pass on the MST.
	GolodetzWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void golodetz_test_B()
{
	// Create the graph.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<9; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 4);
		graph.set_edge_weight(1, 3, 4);
			graph.set_edge_weight(3, 5, 4);
				graph.set_edge_weight(5, 7, 1);
			graph.set_edge_weight(3, 6, 4);
				graph.set_edge_weight(6, 8, 1);
		graph.set_edge_weight(1, 4, 4);
	graph.set_edge_weight(0, 2, 1);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Golodetz waterfall pass on the MST.
	GolodetzWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void golodetz_test_F()
{
	// Create the graph.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<8; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 1);
		graph.set_edge_weight(1, 2, 4);
			graph.set_edge_weight(2, 3, 4);
				graph.set_edge_weight(3, 5, 4);
					graph.set_edge_weight(5, 7, 1);
			graph.set_edge_weight(2, 4, 5);
				graph.set_edge_weight(4, 6, 1);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Golodetz waterfall pass on the MST.
	GolodetzWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void golodetz_test_G()
{
	// Create the graph.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<7; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 4);
		graph.set_edge_weight(1, 3, 4);
		graph.set_edge_weight(1, 4, 4);
			graph.set_edge_weight(4, 5, 4);
			graph.set_edge_weight(4, 6, 4);
	graph.set_edge_weight(0, 2, 4);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Golodetz waterfall pass on the MST.
	GolodetzWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void golodetz_test_H()
{
	// Create the graph.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<7; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 4);
		graph.set_edge_weight(1, 3, 4);
		graph.set_edge_weight(1, 4, 4);
			graph.set_edge_weight(4, 5, 4);
			graph.set_edge_weight(4, 6, 4);
	graph.set_edge_weight(0, 2, 1);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Golodetz waterfall pass on the MST.
	GolodetzWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void golodetz_test_J()
{
	// Create the graph.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<12; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 1);
		graph.set_edge_weight(1, 2, 5);
			graph.set_edge_weight(2, 4, 4);
				graph.set_edge_weight(4, 8, 1);
			graph.set_edge_weight(2, 5, 4);
				graph.set_edge_weight(5, 9, 1);
		graph.set_edge_weight(1, 3, 5);
			graph.set_edge_weight(3, 6, 4);
				graph.set_edge_weight(6, 10, 1);
			graph.set_edge_weight(3, 7, 4);
				graph.set_edge_weight(7, 11, 1);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Golodetz waterfall pass on the MST.
	GolodetzWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void large_test()
{
	// Create the graph.
	AdjacencyGraph<int, int> graph;
	graph.set_node_properties(0, 0);
	graph.set_node_properties(6, 6);
	graph.set_node_properties(13, 13);
	graph.set_node_properties(17, 17);
	graph.set_node_properties(21, 21);
	graph.set_node_properties(24, 24);
	graph.set_node_properties(30, 30);
	graph.set_node_properties(33, 33);
	graph.set_node_properties(62, 62);
	graph.set_node_properties(87, 87);
	graph.set_node_properties(100, 100);
	graph.set_node_properties(108, 108);
	graph.set_node_properties(130, 130);
	graph.set_node_properties(136, 136);
	graph.set_node_properties(179, 179);
	graph.set_node_properties(183, 183);
	graph.set_node_properties(201, 201);
	graph.set_node_properties(256, 256);
	graph.set_node_properties(272, 272);
	graph.set_node_properties(294, 294);
	graph.set_node_properties(311, 311);
	graph.set_node_properties(335, 335);
	graph.set_node_properties(357, 357);
	graph.set_node_properties(362, 362);
	graph.set_node_properties(381, 381);
	graph.set_node_properties(384, 384);
	graph.set_node_properties(403, 403);
	graph.set_node_properties(448, 448);
	graph.set_node_properties(454, 454);
	graph.set_node_properties(461, 461);
	graph.set_node_properties(473, 473);
	graph.set_node_properties(509, 509);
	graph.set_node_properties(570, 570);
	graph.set_node_properties(572, 572);
	graph.set_node_properties(601, 601);
	graph.set_node_properties(693, 693);
	graph.set_node_properties(700, 700);
	graph.set_node_properties(703, 703);
	graph.set_node_properties(729, 729);
	graph.set_node_properties(789, 789);
	graph.set_node_properties(799, 799);
	graph.set_node_properties(841, 841);
	graph.set_node_properties(845, 845);
	graph.set_node_properties(850, 850);
	graph.set_node_properties(858, 858);
	graph.set_node_properties(862, 862);
	graph.set_node_properties(886, 886);
	graph.set_node_properties(914, 914);
	graph.set_node_properties(938, 938);
	graph.set_node_properties(959, 959);
	graph.set_edge_weight(0, 33, 6);
	graph.set_edge_weight(6, 13, 4);
	graph.set_edge_weight(6, 108, 7);
	graph.set_edge_weight(13, 17, 13);
	graph.set_edge_weight(21, 24, 2);
	graph.set_edge_weight(24, 62, 1);
	graph.set_edge_weight(30, 62, 6);
	graph.set_edge_weight(33, 130, 2);
	graph.set_edge_weight(62, 87, 1);
	graph.set_edge_weight(62, 183, 2);
	graph.set_edge_weight(100, 130, 1);
	graph.set_edge_weight(100, 136, 2);
	graph.set_edge_weight(108, 201, 1);
	graph.set_edge_weight(130, 256, 1);
	graph.set_edge_weight(179, 311, 1);
	graph.set_edge_weight(183, 311, 11);
	graph.set_edge_weight(201, 362, 1);
	graph.set_edge_weight(256, 357, 1);
	graph.set_edge_weight(256, 384, 1);
	graph.set_edge_weight(272, 403, 1);
	graph.set_edge_weight(311, 381, 12);
	graph.set_edge_weight(311, 473, 1);
	graph.set_edge_weight(335, 362, 1);
	graph.set_edge_weight(357, 294, 1);
	graph.set_edge_weight(357, 448, 1);
	graph.set_edge_weight(357, 454, 1);
	graph.set_edge_weight(362, 461, 3);
	graph.set_edge_weight(403, 473, 1);
	graph.set_edge_weight(403, 601, 2);
	graph.set_edge_weight(403, 693, 2);
	graph.set_edge_weight(454, 362, 1);
	graph.set_edge_weight(454, 403, 1);
	graph.set_edge_weight(454, 841, 1);
	graph.set_edge_weight(454, 938, 1);
	graph.set_edge_weight(473, 509, 2);
	graph.set_edge_weight(509, 572, 1);
	graph.set_edge_weight(570, 572, 1);
	graph.set_edge_weight(570, 700, 4);
	graph.set_edge_weight(693, 789, 4);
	graph.set_edge_weight(700, 703, 12);
	graph.set_edge_weight(703, 799, 23);
	graph.set_edge_weight(729, 789, 5);
	graph.set_edge_weight(845, 914, 1);
	graph.set_edge_weight(845, 938, 1);
	graph.set_edge_weight(850, 914, 1);
	graph.set_edge_weight(858, 886, 1);
	graph.set_edge_weight(858, 862, 2);
	graph.set_edge_weight(858, 959, 2);
	graph.set_edge_weight(886, 914, 2);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Nicholls waterfall pass on the MST.
	NichollsWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void marcotegui_test()
{
	// Create the graph in the Marcotegui waterfall paper.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<14; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 3);
		graph.set_edge_weight(1, 2, 2);
		graph.set_edge_weight(1, 3, 20);
			graph.set_edge_weight(3, 4, 4);
				graph.set_edge_weight(4, 5, 2);
			graph.set_edge_weight(3, 6, 10);
				graph.set_edge_weight(6, 7, 5);
					graph.set_edge_weight(7, 8, 5);
						graph.set_edge_weight(8, 9, 4);
						graph.set_edge_weight(8, 10, 2);
						graph.set_edge_weight(8, 11, 20);
							graph.set_edge_weight(11, 12, 4);
							graph.set_edge_weight(11, 13, 4);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Marcotegui waterfall pass on the MST.
	MarcoteguiWaterfallPass<int> pass;
	boost::shared_ptr<BasicListener> listener(new BasicListener);
	pass.add_shared_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

template <template <typename> class WaterfallPassType>
void real_image_test(const std::string& filename, const std::string& outputSpecifier,
					 WaterfallPassType<int> pass = WaterfallPassType<int>())
{
	typedef itk::Image<unsigned char,2> UCImage;
	typedef itk::ImageFileReader<UCImage> UCReader;

	// Read in the image (when debugging in VC++, it may be necessary to set the working directory to "$(TargetDir)").
	std::cout << "Loading input image ../resources/" << filename << "...\n";
	UCReader::Pointer reader = UCReader::New();
	reader->SetFileName("../resources/" + filename);
	reader->Update();
	UCImage::Pointer windowedImage = reader->GetOutput();

	std::cout << "Preprocessing input image...\n";

	// Cast the windowed image to make a dummy Hounsfield image.
	typedef itk::Image<int,2> IntImage;
	typedef itk::CastImageFilter<UCImage,IntImage> UC2IntCastFilter;
	UC2IntCastFilter::Pointer uc2intCastFilter = UC2IntCastFilter::New();
	uc2intCastFilter->SetInput(windowedImage);
	uc2intCastFilter->Update();
	IntImage::Pointer hounsfieldImage = uc2intCastFilter->GetOutput();

	// Cast the windowed image to make its pixels real-valued.
	typedef itk::Image<float,2> RealImage;
	typedef itk::CastImageFilter<UCImage,RealImage> UC2RealCastFilter;
	UC2RealCastFilter::Pointer uc2realCastFilter = UC2RealCastFilter::New();
	uc2realCastFilter->SetInput(windowedImage);

	// Smooth this real image using anisotropic diffusion.
	typedef itk::GradientAnisotropicDiffusionImageFilter<RealImage,RealImage> AnisotropicDiffusionFilter;
	AnisotropicDiffusionFilter::Pointer adFilter = AnisotropicDiffusionFilter::New();
	adFilter->SetInput(uc2realCastFilter->GetOutput());
	adFilter->SetConductanceParameter(1.0);
	adFilter->SetNumberOfIterations(15);
	adFilter->SetTimeStep(0.125);

	// Calculate the gradient magnitude of the smoothed image.
	typedef itk::Image<short,2> GradientMagnitudeImage;
	typedef itk::GradientMagnitudeImageFilter<RealImage,GradientMagnitudeImage> GradientMagnitudeFilter;
	GradientMagnitudeFilter::Pointer gradientMagnitudeFilter = GradientMagnitudeFilter::New();
	gradientMagnitudeFilter->SetInput(adFilter->GetOutput());
	gradientMagnitudeFilter->SetUseImageSpacingOff();
	gradientMagnitudeFilter->Update();
	GradientMagnitudeImage::Pointer gradientMagnitudeImage = gradientMagnitudeFilter->GetOutput();

	typedef MeijsterRoerdinkWatershed<GradientMagnitudeImage::PixelType,2> WS;

	// Run the watershed algorithm on the gradient magnitude image.
	std::cout << "Running watershed...\n";
	WS ws(gradientMagnitudeImage, ITKImageUtil::make_4_connected_offsets());
	std::cout << "Layer 0 Node Count: " << ws.label_count() << '\n';

	// Create the initial partition forest.
	std::cout << "Creating initial partition forest...\n";
	shared_ptr<DICOMImageLeafLayer> leafLayer(new DICOMImageLeafLayer(hounsfieldImage, windowedImage, gradientMagnitudeImage));
	shared_ptr<DICOMImageBranchLayer> lowestBranchLayer = IPF::make_lowest_branch_layer(leafLayer, ws.calculate_groups());
	IPF_Ptr ipf(new IPF(leafLayer, lowestBranchLayer));

	// Create a rooted MST from the lowest branch layer.
	std::cout << "Creating rooted MST...\n";
	RootedMST<int> mst(*lowestBranchLayer);

	// Iteratively run a waterfall pass on the MST until the forest is built.
	boost::shared_ptr<IPFConstructionListener> listener(new IPFConstructionListener(ipf));
	pass.add_shared_listener(listener);
	while(mst.node_count() != 1)
	{
		std::cout << "Cloning highest IPF layer...\n";
		ipf->clone_layer(ipf->highest_layer());
		std::cout << "Running waterfall pass...\n";
		pass.run(mst);
		std::cout << "Layer " << ipf->highest_layer() << " Node Count: " << mst.node_count() << '\n';
	}

	// Output the mosaic images for each of the partition forest layers.
	std::cout << "Outputting mosaic images...\n";
	IntImage::SizeType imageSize = hounsfieldImage->GetLargestPossibleRegion().GetSize();
	for(int i=0; i<=ipf->highest_layer(); ++i)
	{
		output_mosaic_image(ipf, i, imageSize[0], imageSize[1], outputSpecifier);
	}
}

int main()
try
{
	//basic_test();
	//comparison_test();
	//golodetz_test_A();
	//golodetz_test_B();
	//golodetz_test_F();
	//golodetz_test_G();
	//golodetz_test_H();
	//golodetz_test_J();
	//large_test();
	//marcotegui_test();
	//real_image_test<NichollsWaterfallPass>("../resources/test.bmp", "../resources/test-partition*.bmp");

	real_image_test<GolodetzWaterfallPass>("baboon.png", "baboon-partition-*-G.png");
	real_image_test<MarcoteguiWaterfallPass>("baboon.png", "baboon-partition-*-M.png");
	real_image_test<NichollsWaterfallPass>("baboon.png", "baboon-partition-*-NC.png", NichollsWaterfallPass<int>(true));
	real_image_test<NichollsWaterfallPass>("baboon.png", "baboon-partition-*-NT.png");

	real_image_test<GolodetzWaterfallPass>("lena.png", "lena-partition-*-G.png");
	real_image_test<MarcoteguiWaterfallPass>("lena.png", "lena-partition-*-M.png");
	real_image_test<NichollsWaterfallPass>("lena.png", "lena-partition-*-NC.png", NichollsWaterfallPass<int>(true));
	real_image_test<NichollsWaterfallPass>("lena.png", "lena-partition-*-NT.png");

	real_image_test<GolodetzWaterfallPass>("pepper.png", "pepper-partition-*-G.png");
	real_image_test<MarcoteguiWaterfallPass>("pepper.png", "pepper-partition-*-M.png");
	real_image_test<NichollsWaterfallPass>("pepper.png", "pepper-partition-*-NC.png", NichollsWaterfallPass<int>(true));
	real_image_test<NichollsWaterfallPass>("pepper.png", "pepper-partition-*-NT.png");

	return 0;
}
catch(std::exception& e)
{
	std::cout << e.what() << std::endl;
}
