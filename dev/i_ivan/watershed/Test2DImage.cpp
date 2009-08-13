#include "MyWatershedSegmenter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScalarToRGBPixelFunctor.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkGDCMImageIO.h"
#include "itkJPEGImageIO.h"
#include <iostream>

int main(int argc, char** argv) {
	if(argc != 3){
		std::cout << "Usage: Test2DImage inputFilename outputFilename"; 
		return 1;
	}
	typedef signed int InputPixelType;
	const unsigned int Dimension = 2;
	typedef itk::Image<InputPixelType, Dimension> InputImageType;
	typedef itk::RGBPixel<unsigned char> RGBPixelType;
	typedef itk::Image<RGBPixelType, 2> RGBImageType;
	typedef itk::ImageFileReader<InputImageType> ReaderType;
	typedef itk::ImageFileWriter<RGBImageType> WriterType;
	typedef itk::GDCMImageIO ImageIOType;
	typedef itk::GradientAnisotropicDiffusionImageFilter<InputImageType,
	InputImageType> DiffusionFilterType;
	typedef itk::GradientMagnitudeImageFilter<InputImageType, InputImageType>
			GradientMagnitudeFilterType;
	typedef itk::Functor::ScalarToRGBPixelFunctor<int> ColorMapFunctorType;
	typedef itk::UnaryFunctorImageFilter<InputImageType,
	RGBImageType, ColorMapFunctorType> ColorMapFilterType;
	typedef itk::JPEGImageIO JImageIOType;

	ReaderType::Pointer reader = ReaderType::New();
	WriterType::Pointer writer = WriterType::New();
	ImageIOType::Pointer GDCMImageIO = ImageIOType::New();
	JImageIOType::Pointer JPEGImageIO = JImageIOType::New();
	ColorMapFilterType::Pointer colormapper = ColorMapFilterType::New();

	reader->SetFileName(argv[1]);
	reader->SetImageIO(GDCMImageIO);

	try {
		reader->Update();
	}
	catch (itk::ExceptionObject & e) {
		std::cerr << "exception in file reader " << std::endl;
		std::cerr << e << std::endl;
		return 1;
	}
	DiffusionFilterType::Pointer diffusion = DiffusionFilterType::New();
	diffusion->SetNumberOfIterations(1);
	diffusion->SetConductanceParameter(4);
	diffusion->SetTimeStep(0.125);
	GradientMagnitudeFilterType::Pointer gradient = GradientMagnitudeFilterType::New();
	diffusion->SetInput(reader->GetOutput());
	gradient->SetInput(diffusion->GetOutput());
	gradient->Update();
	MyWatershedSegmenter<InputImageType> watershed(gradient->GetOutput());
	watershed.buildLowerCompleteImage();
	watershed.buildLabeledImage();
	colormapper->SetInput(watershed.returnFinalImage());
	writer->SetInput(colormapper->GetOutput());
	writer->UseInputMetaDataDictionaryOff();
	writer->SetImageIO(JPEGImageIO);
	writer->SetFileName(argv[2]);
	writer->Update();
}
