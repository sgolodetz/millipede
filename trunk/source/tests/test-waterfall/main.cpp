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
#include <common/partitionforests/images/IPFUtil.h>
#include <common/segmentation/CTIPFBuilder.h>
#include <common/segmentation/waterfall/NichollsWaterfallPass.h>
#include <common/segmentation/watershed/MeijsterRoerdinkWatershed.h>
using namespace mp;

typedef PartitionForest<CTImageLeafLayer,CTImageBranchLayer> IPF;
typedef shared_ptr<IPF> IPF_Ptr;

//#################### HELPER FUNCTIONS ####################
void output_mosaic_image(const IPF_Ptr& ipf, int layerIndex, int width, int height)
{
	typedef itk::Image<unsigned char,2> Image;
	Image::Pointer image = layerIndex > 0 ?
		IPFUtil::make_mosaic_image_with_boundaries(ipf, layerIndex, width, height) :
		IPFUtil::make_mosaic_image(ipf, layerIndex, width, height);

	typedef itk::ImageFileWriter<Image> Writer;
	Writer::Pointer writer = Writer::New();
	writer->SetInput(image);
	writer->SetFileName(OSSWrapper() << "../resources/partition" << layerIndex << ".bmp");
	writer->Update();
}

//#################### TEST FUNCTIONS ####################
struct BasicListener : WaterfallPass<int>::Listener
{
	void merge_nodes(int u, int v)
	{
		std::cout << "Merging nodes " << u << " and " << v << '\n';
	}
};

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
	pass.add_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

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
	typedef itk::Image<int,2> GradientMagnitudeImage;
	typedef itk::GradientMagnitudeImageFilter<RealImage,GradientMagnitudeImage> GradientMagnitudeFilter;
	GradientMagnitudeFilter::Pointer gradientMagnitudeFilter = GradientMagnitudeFilter::New();
	gradientMagnitudeFilter->SetInput(adFilter->GetOutput());
	gradientMagnitudeFilter->SetUseImageSpacingOff();
	gradientMagnitudeFilter->Update();
	GradientMagnitudeImage::Pointer gradientMagnitudeImage = gradientMagnitudeFilter->GetOutput();

	typedef MeijsterRoerdinkWatershed<int,2> WS;

	// Specify the necessary offsets for 4-connectivity.
	WS::NeighbourOffsets offsets(4);
	offsets[0][0] = 0;		offsets[0][1] = -1;		// above
	offsets[1][0] = -1;		offsets[1][1] = 0;		// left
	offsets[2][0] = 1;		offsets[2][1] = 0;		// right
	offsets[3][0] = 0;		offsets[3][1] = 1;		// below

	// Run the watershed algorithm on the gradient magnitude image.
	std::cout << "Running watershed...\n";
	WS ws(gradientMagnitudeImage, offsets);
	std::cout << "Layer 0 Node Count: " << ws.label_count() << '\n';

	// Create the initial partition forest.
	std::cout << "Creating initial partition forest...\n";
	shared_ptr<CTImageLeafLayer> leafLayer(new CTImageLeafLayer(hounsfieldImage, windowedImage, gradientMagnitudeImage));
	shared_ptr<CTImageBranchLayer> lowestBranchLayer = IPF::make_lowest_branch_layer(leafLayer, ws.calculate_groups());
	IPF_Ptr ipf(new IPF(leafLayer, lowestBranchLayer));

	// Create a rooted MST from the lowest branch layer.
	std::cout << "Creating rooted MST...\n";
	RootedMST<int> mst(*lowestBranchLayer);

	// Iteratively run a Nicholls waterfall pass on the MST until the forest is built.
	NichollsWaterfallPass<int> pass;
	boost::shared_ptr<IPFConstructionListener> listener(new IPFConstructionListener(ipf));
	pass.add_listener(listener);
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

void job_test()
{
	typedef itk::Image<int,3> Image;
	typedef itk::ImageFileReader<Image> Reader;

	// Read in the image (when debugging in VC++, it may be necessary to set the working directory to "$(TargetDir)").
	std::cout << "Loading input image...\n";
	Reader::Pointer reader = Reader::New();
	reader->SetFileName("../resources/test.bmp");
	reader->Update();
	Image::Pointer image = reader->GetOutput();

	// Create a DICOM volume (obviously not a proper one, as the image being read in is actually a greyscale one).
	DICOMVolume_Ptr volume(new DICOMVolume(image));

	// Set the segmentation options.
	itk::Size<3> size = image->GetLargestPossibleRegion().GetSize();
	WindowSettings windowSettings(40, 400);	// dummy window settings
	CTSegmentationOptions options(CTSegmentationOptions::INPUTTYPE_HOUNSFIELD, size, 10, windowSettings);

	// Build the IPF.
	std::cout << "Building IPF...\n";
	typedef CTIPFBuilder::IPF_Ptr IPF_Ptr;
	IPF_Ptr ipf;
	Job_Ptr job(new CTIPFBuilder(volume, options, ipf));
	Job::execute_in_thread(job);
	while(!job->is_finished());

	// Output the mosaic images for each of the partition forest layers.
	std::cout << "Outputting mosaic images...\n";
	for(int i=0; i<=ipf->highest_layer(); ++i)
	{
		output_mosaic_image(ipf, i, size[0], size[1]);
	}
}

int main()
try
{
	//basic_test();
	real_image_test();
	//job_test();
	return 0;
}
catch(std::exception& e)
{
	std::cout << e.what() << std::endl;
}
