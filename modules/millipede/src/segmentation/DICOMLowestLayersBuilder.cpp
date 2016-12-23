/***
 * millipede: DICOMLowestLayersBuilder.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "segmentation/DICOMLowestLayersBuilder.h"

#include <itkCastImageFilter.h>
#include <itkGradientAnisotropicDiffusionImageFilter.h>
#include <itkGradientMagnitudeImageFilter.h>

#include "dicom/volumes/DICOMVolume.h"
#include "exceptions/Exception.h"
#include "segmentation/watershed/MeijsterRoerdinkWatershed.h"
#include "util/ITKImageUtil.h"

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMLowestLayersBuilder::DICOMLowestLayersBuilder(const DICOMSegmentationOptions& segmentationOptions, DICOMImageLeafLayer_Ptr& leafLayer,
												   DICOMImageBranchLayer_Ptr& lowestBranchLayer)
:	m_leafLayer(leafLayer), m_lowestBranchLayer(lowestBranchLayer), m_segmentationOptions(segmentationOptions)
{}

//#################### PUBLIC METHODS ####################
int DICOMLowestLayersBuilder::length() const
{
	return m_segmentationOptions.adfIterations + 3;
}

void DICOMLowestLayersBuilder::set_volume_hook(const DataHook<DICOMVolume_CPtr>& volumeHook)
{
	m_volumeHook = volumeHook;
}

//#################### PRIVATE METHODS ####################
void DICOMLowestLayersBuilder::execute_impl()
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

	// Cast the input image (whether base or windowed) to make its pixels real-valued.
	RealImage::Pointer realImage;
	switch(m_segmentationOptions.inputType)
	{
		case DICOMSegmentationOptions::INPUTTYPE_BASE:
		{
			typedef itk::CastImageFilter<BaseImage,RealImage> CastFilter;
			CastFilter::Pointer castFilter = CastFilter::New();
			castFilter->SetInput(baseImage);
			castFilter->Update();
			realImage = castFilter->GetOutput();
			break;
		}
		case DICOMSegmentationOptions::INPUTTYPE_WINDOWED:
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
			throw Exception("Unknown segmentation input type");		// this should never happen
		}
	}
	if(is_aborted()) return;

	// Smooth this real image using anisotropic diffusion filtering.
	typedef itk::GradientAnisotropicDiffusionImageFilter<RealImage,RealImage> ADFilter;
	for(int i=0; i<m_segmentationOptions.adfIterations; ++i)
	{
		ADFilter::Pointer adFilter = ADFilter::New();
		adFilter->SetInput(realImage);
		adFilter->SetConductanceParameter(m_segmentationOptions.adfConductance);
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

	m_leafLayer.reset(new DICOMImageLeafLayer(baseImage, windowedImage, gradientMagnitudeImage));
	if(is_aborted()) return;
	m_lowestBranchLayer = IPF::make_lowest_branch_layer(m_leafLayer, ws.calculate_groups());
	if(is_aborted()) return;
}

}
