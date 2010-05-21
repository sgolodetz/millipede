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

#include <common/partitionforests/base/PartitionForest.h>
#include <common/partitionforests/images/CTImageUtil.h>
#include <common/segmentation/waterfall/NichollsWaterfallPass.h>
#include <common/segmentation/watershed/MeijsterRoerdinkWatershed.h>
using namespace mp;

typedef PartitionForest<CTImageLeafLayer,CTImageBranchLayer> IPF;
typedef shared_ptr<IPF> IPF_Ptr;

//#################### HELPER FUNCTIONS ####################
void output_partition(const IPF_Ptr& ipf, int layerIndex)
{
	// TODO
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
		// TODO: Merge the regions in the top-most layer of the IPF.
		//std::cout << "Merging nodes " << u << " and " << v << '\n';
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
	adFilter->SetNumberOfIterations(5);		// a typical value (see the ITK software guide)
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
	shared_ptr<CTImageLeafLayer> leafLayer = CTImageUtil::make_leaf_layer(hounsfieldImage, windowedImage);
	shared_ptr<CTImageBranchLayer> lowestBranchLayer = IPF::construct_lowest_branch_layer(leafLayer, ws.calculate_groups());
	IPF_Ptr ipf(new IPF(leafLayer, lowestBranchLayer));

	// Create a rooted MST from the lowest branch layer.
	std::cout << "Creating rooted MST...\n";
	RootedMST<int> mst(*lowestBranchLayer);

	// Iteratively run a Nicholls waterfall pass on the MST until the tree is built.
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
		output_partition(ipf, ipf->highest_layer());
	}
}

int main()
try
{
	//basic_test();
	real_image_test();
	return 0;
}
catch(std::exception& e)
{
	std::cout << e.what() << std::endl;
}
