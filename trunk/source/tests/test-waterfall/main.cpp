/***
 * test-waterfall: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>

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

		unsigned char mosaicValue;
		if(regionBoundary)		mosaicValue = std::numeric_limits<unsigned char>::max();
		else if(layerIndex > 0)	mosaicValue = static_cast<unsigned char>(ipf->branch_properties(it.GetCenterPixel()).mean_grey_value());
		else					mosaicValue = ipf->leaf_properties(it.GetCenterPixel().index()).grey_value();
		mosaicImage->SetPixel(it.GetIndex(), mosaicValue);
	}

	return mosaicImage;
}

void output_mosaic_image(const IPF_Ptr& ipf, int layerIndex, int width, int height)
{
	typedef itk::Image<unsigned char,2> Image;
	Image::Pointer image = layerIndex > 0 ?
		make_mosaic_image_with_boundaries(ipf, layerIndex, width, height) :
		make_mosaic_image(ipf, layerIndex, width, height);

	typedef itk::ImageFileWriter<Image> Writer;
	Writer::Pointer writer = Writer::New();
	writer->SetInput(image);
	writer->SetFileName(OSSWrapper() << "../resources/partition" << layerIndex << ".bmp");
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

void real_image_test()
{
	typedef itk::Image<unsigned char,2> UCImage;
	typedef itk::ImageFileReader<UCImage> UCReader;

	// Read in the image (when debugging in VC++, it may be necessary to set the working directory to "$(TargetDir)").
	std::cout << "Loading input image...\n";
	UCReader::Pointer reader = UCReader::New();
	reader->SetFileName("../resources/test.bmp");
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

	// Iteratively run a Nicholls waterfall pass on the MST until the forest is built.
	NichollsWaterfallPass<int> pass;
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
		output_mosaic_image(ipf, i, imageSize[0], imageSize[1]);
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
	golodetz_test_J();
	//marcotegui_test();
	//real_image_test();
	return 0;
}
catch(std::exception& e)
{
	std::cout << e.what() << std::endl;
}
