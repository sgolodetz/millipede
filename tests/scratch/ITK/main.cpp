/***
 * scratchtest_ITK: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <boost/lexical_cast.hpp>
using boost::lexical_cast;

#include "itkCastImageFilter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkScalarToRGBPixelFunctor.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkWatershedImageFilter.h"

#include <srgutil/filesystem/PathFinder.h>
using namespace srgutil;

namespace bf = boost::filesystem;

int main()
{
	typedef itk::RGBPixel<unsigned char> RGBPixelType;

	typedef itk::Image<unsigned char> UCImageType;
	typedef itk::Image<float> RealImageType;
	typedef itk::Image<itk::IdentifierType> LabelledImageType;
	typedef itk::Image<RGBPixelType> RGBImageType;

	typedef itk::ImageFileReader<UCImageType> UCFileReader;
	typedef itk::CastImageFilter<UCImageType,RealImageType> UCToRealCaster;
	typedef itk::GradientAnisotropicDiffusionImageFilter<RealImageType,RealImageType> Diffuser;

	typedef itk::RescaleIntensityImageFilter<RealImageType,UCImageType> RealToUCRescaler;
	typedef itk::ImageFileWriter<UCImageType> UCFileWriter;

	typedef itk::GradientMagnitudeImageFilter<RealImageType,RealImageType> GradientMagnitude;
	typedef itk::WatershedImageFilter<RealImageType> Watershed;
	typedef itk::Functor::ScalarToRGBPixelFunctor<itk::IdentifierType> ColourMapFunctor;
	typedef itk::UnaryFunctorImageFilter<LabelledImageType,RGBImageType,ColourMapFunctor> ColourMapper;
	typedef itk::ImageFileWriter<RGBImageType> RGBFileWriter;

  const bf::path outputDir = find_subdir_from_executable("output");
	const bf::path resourcesDir = find_subdir_from_executable("resources");
	bf::create_directories(outputDir);

	// Set up the file reader.
	UCFileReader::Pointer reader = UCFileReader::New();
	reader->SetFileName((resourcesDir / "test.bmp").string());

	// Set up the caster.
	UCToRealCaster::Pointer caster = UCToRealCaster::New();
	caster->SetInput(reader->GetOutput());

	// Set up the diffuser.
	Diffuser::Pointer diffuser = Diffuser::New();
	diffuser->SetInput(caster->GetOutput());
	diffuser->SetNumberOfIterations(20);
	diffuser->SetConductanceParameter(0.5);
	diffuser->SetTimeStep(0.125);

	// Set up the rescaler.
	RealToUCRescaler::Pointer rescaler = RealToUCRescaler::New();
	rescaler->SetInput(diffuser->GetOutput());
	rescaler->SetOutputMinimum(0);
	rescaler->SetOutputMaximum(255);

	// Set up the file writer to write the diffusion results to file.
	UCFileWriter::Pointer diffusionWriter = UCFileWriter::New();
	diffusionWriter->SetInput(rescaler->GetOutput());
	diffusionWriter->SetFileName((outputDir / "test-diffusion.bmp").string());
	diffusionWriter->Update();

	// Set up the gradient magnitude filter.
	GradientMagnitude::Pointer gradientMagnitude = GradientMagnitude::New();
	gradientMagnitude->SetInput(diffuser->GetOutput());

	// Set up the watershed filter.
	Watershed::Pointer watershed = Watershed::New();
	watershed->SetInput(gradientMagnitude->GetOutput());
	watershed->SetThreshold(0);

	// Set up the colour mapper.
	ColourMapper::Pointer colourMapper = ColourMapper::New();
	colourMapper->SetInput(watershed->GetOutput());

	// Write images for various watershed levels to disk.
	for(int i=0; i<=5; ++i)
	{
		double d = i*0.1;
		watershed->SetLevel(d);

		RGBFileWriter::Pointer watershedWriter = RGBFileWriter::New();
		watershedWriter->SetInput(colourMapper->GetOutput());
		watershedWriter->SetFileName((outputDir / ("test-watershed-" + lexical_cast<std::string,int>(i) + ".bmp")).string());
		try
		{
			watershedWriter->Update();
		}
		catch(itk::ExceptionObject& e)
		{
			std::cout << e << std::endl;
		}
	}

	return 0;
}