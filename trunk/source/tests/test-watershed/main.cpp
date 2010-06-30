/***
 * test-watershed: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <cassert>
#include <iostream>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <itkCastImageFilter.h>
#include <itkGradientAnisotropicDiffusionImageFilter.h>
#include <itkGradientMagnitudeImageFilter.h>
#include <itkImageFileReader.h>
#include <itkImageFileWriter.h>
#include <itkScalarToRGBPixelFunctor.h>

#include <common/partitionforests/base/PartitionForest.h>
#include <common/partitionforests/images/CTMRImageBranchLayer.h>
#include <common/partitionforests/images/CTMRImageLeafLayer.h>
#include <common/segmentation/watershed/MeijsterRoerdinkWatershed.h>
#include <common/util/ITKImageUtil.h>
using namespace mp;

//#################### TEST FUNCTIONS ####################
void basic_test()
{
	typedef itk::Image<int,2> Image;

	int pixels[] =
	{
		2,2,3,4,4,1,1,
		2,2,3,4,4,2,2,
		4,4,5,5,5,4,6,
		6,6,5,5,5,2,6,
		6,6,5,5,5,2,1
	};

	Image::Pointer image = ITKImageUtil::make_filled_image(7, 5, pixels);
	ITKImageUtil::output_image(std::cout, image);
	std::cout << '\n';

	// Run the watershed algorithm on the image.
	typedef MeijsterRoerdinkWatershed<int,2> WS;
	WS ws(image, ITKImageUtil::make_4_connected_offsets());

	// Output the results.
	ITKImageUtil::output_image(std::cout, ws.lower_complete());
	std::cout << '\n';

	ITKImageUtil::output_image(std::cout, ws.arrows());
	std::cout << '\n';

	ITKImageUtil::output_image(std::cout, ws.labels());
	std::cout << '\n';
}

void gradient_test()
{
	typedef itk::Image<int,2> Image;

	int pixels[] =
	{
		2,2,2,9,9,3,3,3,
		2,2,2,9,9,3,3,3,
		2,2,2,9,9,3,3,3,
		9,9,9,9,9,9,9,9,
		9,9,9,9,9,9,9,9,
		5,5,5,9,9,3,3,3,
		5,5,5,9,9,3,3,3,
		5,5,5,9,9,3,3,3,
	};

	Image::Pointer image = ITKImageUtil::make_filled_image(8, 8, pixels);
	ITKImageUtil::output_image(std::cout, image);
	std::cout << '\n';

	typedef itk::GradientMagnitudeImageFilter<Image,Image> GradientMagnitudeFilter;
	GradientMagnitudeFilter::Pointer gradientMagnitudeFilter = GradientMagnitudeFilter::New();
	gradientMagnitudeFilter->SetInput(image);
	gradientMagnitudeFilter->SetUseImageSpacingOff();
	gradientMagnitudeFilter->Update();

	Image::Pointer gradientMagnitudeImage = gradientMagnitudeFilter->GetOutput();
	ITKImageUtil::output_image(std::cout, gradientMagnitudeImage);
	std::cout << '\n';

	// Run the watershed algorithm on the gradient magnitude image.
	typedef MeijsterRoerdinkWatershed<int,2> WS;
	WS ws(gradientMagnitudeImage, ITKImageUtil::make_4_connected_offsets());

	// Output the results.
	ITKImageUtil::output_image(std::cout, ws.lower_complete());
	std::cout << '\n';

	ITKImageUtil::output_image(std::cout, ws.arrows());
	std::cout << '\n';

	ITKImageUtil::output_image(std::cout, ws.labels());
	std::cout << '\n';
}

void forest_test()
{
	// Create the Hounsfield and 'windowed' images.
	typedef itk::Image<int,2> HounsfieldImage;
	typedef itk::Image<unsigned char,2> WindowedImage;
	typedef itk::Image<short,2> GradientMagnitudeImage;

	int pixels[] =
	{
		2,2,2,9,9,3,3,3,
		2,2,2,9,9,3,3,3,
		2,2,2,9,9,3,3,3,
		9,9,9,9,9,9,9,9,
		9,9,9,9,9,9,9,9,
		5,5,5,9,9,3,3,3,
		5,5,5,9,9,3,3,3,
		5,5,5,9,9,3,3,3,
	};

	HounsfieldImage::Pointer hounsfieldImage = ITKImageUtil::make_filled_image(8, 8, pixels);
	ITKImageUtil::output_image(std::cout, hounsfieldImage);
	std::cout << '\n';

	// This is not the proper way to do windowing - it's a hack for testing purposes.
	typedef itk::CastImageFilter<HounsfieldImage,WindowedImage> CastFilter;
	CastFilter::Pointer castFilter = CastFilter::New();
	castFilter->SetInput(hounsfieldImage);
	castFilter->Update();
	WindowedImage::Pointer windowedImage = castFilter->GetOutput();

	// Calculate the gradient magnitude image.
	typedef itk::GradientMagnitudeImageFilter<WindowedImage,GradientMagnitudeImage> GradientMagnitudeFilter;
	GradientMagnitudeFilter::Pointer gradientMagnitudeFilter = GradientMagnitudeFilter::New();
	gradientMagnitudeFilter->SetInput(windowedImage);
	gradientMagnitudeFilter->SetUseImageSpacingOff();
	gradientMagnitudeFilter->Update();

	GradientMagnitudeImage::Pointer gradientMagnitudeImage = gradientMagnitudeFilter->GetOutput();
	ITKImageUtil::output_image(std::cout, gradientMagnitudeImage);
	std::cout << '\n';

	// Run the watershed algorithm on the gradient magnitude image.
	typedef MeijsterRoerdinkWatershed<GradientMagnitudeImage::PixelType,2> WS;
	WS ws(gradientMagnitudeImage, ITKImageUtil::make_4_connected_offsets());

	// Output the results.
	ITKImageUtil::output_image(std::cout, ws.lower_complete());
	std::cout << '\n';

	ITKImageUtil::output_image(std::cout, ws.arrows());
	std::cout << '\n';

	ITKImageUtil::output_image(std::cout, ws.labels());
	std::cout << '\n';

	// Create the initial partition forest.
	typedef PartitionForest<CTMRImageLeafLayer,CTMRImageBranchLayer> IPF;
	typedef shared_ptr<IPF> IPF_Ptr;
	shared_ptr<CTMRImageLeafLayer> leafLayer(new CTMRImageLeafLayer(hounsfieldImage, windowedImage, gradientMagnitudeImage));
	shared_ptr<CTMRImageBranchLayer> lowestBranchLayer = IPF::make_lowest_branch_layer(leafLayer, ws.calculate_groups());
	std::copy(lowestBranchLayer->edges_cbegin(), lowestBranchLayer->edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
	IPF_Ptr ipf(new IPF(leafLayer, lowestBranchLayer));
}

void real_image_test()
{
	typedef itk::Image<unsigned char,2> UCImage;
	typedef itk::ImageFileReader<UCImage> UCReader;

	// Read in the image (when debugging in VC++, it may be necessary to set the working directory to "$(TargetDir)").
	UCReader::Pointer reader = UCReader::New();
	reader->SetFileName("../resources/test.bmp");
	reader->Update();
	UCImage::Pointer windowedImage = reader->GetOutput();

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
	typedef itk::Image<short,2> GradientMagnitudeImage;
	typedef itk::GradientMagnitudeImageFilter<RealImage,GradientMagnitudeImage> GradientMagnitudeFilter;
	GradientMagnitudeFilter::Pointer gradientMagnitudeFilter = GradientMagnitudeFilter::New();
	gradientMagnitudeFilter->SetInput(adFilter->GetOutput());
	gradientMagnitudeFilter->SetUseImageSpacingOff();
	gradientMagnitudeFilter->Update();
	GradientMagnitudeImage::Pointer gradientMagnitudeImage = gradientMagnitudeFilter->GetOutput();

	// Run the watershed algorithm on the gradient magnitude image.
	typedef MeijsterRoerdinkWatershed<GradientMagnitudeImage::PixelType,2> WS;
	WS ws(gradientMagnitudeImage, ITKImageUtil::make_4_connected_offsets());

	std::cout << "Label Count: " << ws.label_count() << '\n';

	// Convert the watershed label image to a colour RGB image.
	typedef itk::RGBPixel<unsigned char> RGBPixelType;
	typedef itk::Image<RGBPixelType,2> RGBImage;
	typedef itk::Functor::ScalarToRGBPixelFunctor<long> ColourMapFunctorType;
	typedef itk::UnaryFunctorImageFilter<WS::LabelImage, RGBImage, ColourMapFunctorType> ColourMapFilter;
	ColourMapFilter::Pointer colourMapper = ColourMapFilter::New();
	colourMapper->SetInput(ws.labels());

	// Output the result to a file.
	typedef itk::ImageFileWriter<RGBImage> Writer;
	Writer::Pointer writer = Writer::New();
	writer->SetInput(colourMapper->GetOutput());
	writer->SetFileName("../resources/output.bmp");
	writer->Update();

	// Create the initial partition forest.
	typedef PartitionForest<CTMRImageLeafLayer,CTMRImageBranchLayer> IPF;
	typedef shared_ptr<IPF> IPF_Ptr;
	shared_ptr<CTMRImageLeafLayer> leafLayer(new CTMRImageLeafLayer(hounsfieldImage, windowedImage, gradientMagnitudeImage));
	shared_ptr<CTMRImageBranchLayer> lowestBranchLayer = IPF::make_lowest_branch_layer(leafLayer, ws.calculate_groups());
	IPF_Ptr ipf(new IPF(leafLayer, lowestBranchLayer));
}

int main()
try
{
	//basic_test();
	//gradient_test();
	//forest_test();
	real_image_test();
	return 0;
}
catch(std::exception& e)
{
	std::cout << e.what() << std::endl;
}
