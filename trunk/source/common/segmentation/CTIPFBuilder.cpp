/***
 * millipede: CTIPFBuilder.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTIPFBuilder.h"

#include <itkCastImageFilter.h>
#include <itkGradientAnisotropicDiffusionImageFilter.h>
#include <itkGradientMagnitudeImageFilter.h>

#include <common/adts/RootedMST.h>
#include <common/dicom/volumes/DICOMVolume.h>
#include <common/exceptions/Exception.h>
#include <common/segmentation/waterfall/NichollsWaterfallPass.h>
#include <common/segmentation/watershed/MeijsterRoerdinkWatershed.h>
#include "ForestBuildingWaterfallPassListener.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CTIPFBuilder::CTIPFBuilder(const DICOMVolume_CPtr& volume, const CTSegmentationOptions& segmentationOptions, CTIPF_Ptr& ipf)
:	m_ipf(ipf), m_segmentationOptions(segmentationOptions), m_volume(volume)
{}

//#################### PUBLIC METHODS ####################
void CTIPFBuilder::execute()
{
	typedef itk::Image<int,3> GradientMagnitudeImage;
	typedef itk::Image<int,3> HounsfieldImage;
	typedef itk::Image<float,3> RealImage;
	typedef itk::Image<unsigned char,3> WindowedImage;

	HounsfieldImage::Pointer hounsfieldImage = m_volume->base_image();

	set_status("Preprocessing image...");

	// Construct the windowed image.
	WindowedImage::Pointer windowedImage = m_volume->windowed_image(m_segmentationOptions.windowSettings);

	// Cast the input image (whether Hounsfield or windowed) to make its pixels real-valued.
	RealImage::Pointer realImage;
	switch(m_segmentationOptions.inputType)
	{
		case CTSegmentationOptions::INPUTTYPE_HOUNSFIELD:
		{
			typedef itk::CastImageFilter<HounsfieldImage,RealImage> CastFilter;
			CastFilter::Pointer castFilter = CastFilter::New();
			castFilter->SetInput(hounsfieldImage);
			castFilter->Update();
			realImage = castFilter->GetOutput();
			break;
		}
		case CTSegmentationOptions::INPUTTYPE_WINDOWED:
		{
			typedef itk::CastImageFilter<WindowedImage,RealImage> CastFilter;
			CastFilter::Pointer castFilter = CastFilter::New();
			castFilter->SetInput(windowedImage);
			castFilter->Update();
			realImage = castFilter->GetOutput();
			break;
		}
		default:
		{
			throw Exception("Unknown CT segmentation input type");	// this should never happen
		}
	}

	// Set up an anisotropic diffusion filter to smooth this real image.
	typedef itk::GradientAnisotropicDiffusionImageFilter<RealImage,RealImage> ADFilter;
	ADFilter::Pointer adFilter = ADFilter::New();
	adFilter->SetInput(realImage);
	adFilter->SetConductanceParameter(1.0);
	adFilter->SetNumberOfIterations(15);
	adFilter->SetTimeStep(0.125);

	// Calculate the gradient magnitude of the smoothed image.
	typedef itk::GradientMagnitudeImageFilter<RealImage,GradientMagnitudeImage> GMFilter;
	GMFilter::Pointer gmFilter = GMFilter::New();
	gmFilter->SetInput(adFilter->GetOutput());
	gmFilter->SetUseImageSpacingOff();
	gmFilter->Update();
	GradientMagnitudeImage::Pointer gradientMagnitudeImage = gmFilter->GetOutput();

	set_progress(1);
	set_status("Running watershed...");

	typedef MeijsterRoerdinkWatershed<GradientMagnitudeImage::PixelType,3> WS;

	// Specify the necessary offsets for 6-connectivity.
	WS::NeighbourOffsets offsets(6);
	offsets[0][0] = 0;	offsets[0][1] = 0;	offsets[0][2] = -1;
	offsets[1][0] = 0;	offsets[1][1] = -1;	offsets[1][2] = 0;
	offsets[2][0] = -1;	offsets[2][1] = 0;	offsets[2][2] = 0;
	offsets[3][0] = 1;	offsets[3][1] = 0;	offsets[3][2] = 0;
	offsets[4][0] = 0;	offsets[4][1] = 1;	offsets[4][2] = 0;
	offsets[5][0] = 0;	offsets[5][1] = 0;	offsets[5][2] = 1;

	// Run the watershed algorithm on the gradient magnitude image.
	WS ws(gradientMagnitudeImage, offsets);

	set_progress(2);
	set_status("Creating initial partition forest...");

	boost::shared_ptr<CTImageLeafLayer> leafLayer(new CTImageLeafLayer(hounsfieldImage, windowedImage, gradientMagnitudeImage));
	boost::shared_ptr<CTImageBranchLayer> lowestBranchLayer = CTIPF::make_lowest_branch_layer(leafLayer, ws.calculate_groups());
	CTIPF_Ptr ipf(new CTIPF(leafLayer, lowestBranchLayer));

	set_progress(3);
	set_status("Creating rooted MST for lowest branch layer...");

	RootedMST<int> mst(*lowestBranchLayer);

	set_progress(4);
	set_status("Running waterfall...");

	// Iteratively run a Nicholls waterfall pass on the MST until the forest is built.
	typedef WaterfallPass<int>::Listener WaterfallPassListener;
	NichollsWaterfallPass<int> waterfallPass;
	boost::shared_ptr<WaterfallPassListener> listener = make_forest_building_waterfall_pass_listener(ipf);
	waterfallPass.add_listener(listener);
	while(mst.node_count() != 1 && ipf->highest_layer() < m_segmentationOptions.waterfallLayerLimit)
	{
		ipf->clone_layer(ipf->highest_layer());
		waterfallPass.run(mst);
	}

	set_finished();
}

int CTIPFBuilder::length() const
{
	return 5;
}

}
