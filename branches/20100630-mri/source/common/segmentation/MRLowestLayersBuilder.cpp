/***
 * millipede: MRLowestLayersBuilder.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MRLowestLayersBuilder.h"

#include <itkCastImageFilter.h>
#include <itkGradientAnisotropicDiffusionImageFilter.h>
#include <itkGradientMagnitudeImageFilter.h>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/exceptions/Exception.h>
#include <common/segmentation/watershed/MeijsterRoerdinkWatershed.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### CONSTRUCTORS ####################
MRLowestLayersBuilder::MRLowestLayersBuilder(const MRSegmentationOptions& segmentationOptions, CTMRImageLeafLayer_Ptr& leafLayer,
											 CTMRImageBranchLayer_Ptr& lowestBranchLayer)
:	m_leafLayer(leafLayer), m_lowestBranchLayer(lowestBranchLayer), m_segmentationOptions(segmentationOptions)
{}

//#################### PUBLIC METHODS ####################
void MRLowestLayersBuilder::execute()
{
	typedef itk::Image<int,3> BaseImage;
	typedef itk::Image<short,3> GradientMagnitudeImage;
	typedef itk::Image<float,3> RealImage;
	typedef itk::Image<unsigned char,3> WindowedImage;

	BaseImage::Pointer baseImage = m_volumeHook.get()->base_image();

	//~~~~~~~
	// STEP 1
	//~~~~~~~

	set_status("Preprocessing image...");

	// Construct the windowed image.
	WindowedImage::Pointer windowedImage = m_volumeHook.get()->windowed_image(m_segmentationOptions.windowSettings);
	if(is_aborted()) return;

	// Cast the windowed image to make its pixels real-valued.
	typedef itk::CastImageFilter<WindowedImage,RealImage> CastFilter;
	CastFilter::Pointer castFilter = CastFilter::New();
	castFilter->SetInput(windowedImage);
	castFilter->Update();
	RealImage::Pointer realImage = castFilter->GetOutput();
	if(is_aborted()) return;

	// Smooth this real image using anisotropic diffusion filtering.
	typedef itk::GradientAnisotropicDiffusionImageFilter<RealImage,RealImage> ADFilter;
	for(int i=0; i<m_segmentationOptions.adfIterations; ++i)
	{
		ADFilter::Pointer adFilter = ADFilter::New();
		adFilter->SetInput(realImage);
		adFilter->SetConductanceParameter(1.0);
		adFilter->SetNumberOfIterations(1);
		adFilter->SetTimeStep(0.0625);
		adFilter->Update();
		realImage = adFilter->GetOutput();

		if(is_aborted()) return;
		increment_progress();
	}

	// Calculate the gradient magnitude of the smoothed image.
	typedef itk::GradientMagnitudeImageFilter<RealImage,GradientMagnitudeImage> GMFilter;
	GMFilter::Pointer gmFilter = GMFilter::New();
	gmFilter->SetInput(realImage);
	gmFilter->SetUseImageSpacingOff();
	gmFilter->Update();
	GradientMagnitudeImage::Pointer gradientMagnitudeImage = gmFilter->GetOutput();

	if(is_aborted()) return;
	increment_progress();

	//~~~~~~~
	// STEP 2
	//~~~~~~~

	set_status("Running watershed...");

	// Run the watershed algorithm on the gradient magnitude image.
	typedef MeijsterRoerdinkWatershed<GradientMagnitudeImage::PixelType,3> WS;
	WS ws(gradientMagnitudeImage, ITKImageUtil::make_6_connected_offsets());

	if(is_aborted()) return;
	increment_progress();

	//~~~~~~~
	// STEP 3
	//~~~~~~~

	set_status("Creating lowest forest layers...");

	m_leafLayer.reset(new CTMRImageLeafLayer(baseImage, windowedImage, gradientMagnitudeImage));
	if(is_aborted()) return;
	m_lowestBranchLayer = IPF::make_lowest_branch_layer(m_leafLayer, ws.calculate_groups());
	
	if(is_aborted()) return;
	set_finished();
}

int MRLowestLayersBuilder::length() const
{
	return m_segmentationOptions.adfIterations + 3;
}

void MRLowestLayersBuilder::set_volume_hook(const DataHook<DICOMVolume_CPtr>& volumeHook)
{
	m_volumeHook = volumeHook;
}

}
