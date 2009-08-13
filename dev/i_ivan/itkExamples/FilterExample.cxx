#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkGDCMImageIO.h"

int main() {

	typedef signed short 	InputPixelType;
	const unsigned int 	Dimension = 2;

	typedef itk::Image<InputPixelType, Dimension> InputImageType;
	typedef itk::ImageFileReader<InputImageType> ReaderType;
	typedef itk::BinaryThresholdImageFilter<InputImageType, InputImageType> FilterType;
	typedef itk::ImageFileWriter<InputImageType> WriterType;
	typedef itk::GDCMImageIO 	ImageIOType;

	ReaderType::Pointer reader = ReaderType::New();
	FilterType::Pointer filter = FilterType::New();
	WriterType::Pointer writer = WriterType::New();
	ImageIOType::Pointer gdcmImageIO = ImageIOType::New();

	const char * filename = "images/IM50";
	reader->SetFileName(filename);
	reader->SetImageIO(gdcmImageIO);

	try {
		reader->Update();
	}
	catch (itk::ExceptionObject & e) {
		std::cerr << "exception in file reader " << std::endl;
		std::cerr << e << std::endl;
		return 1;
	}
	filter->SetInput(reader->GetOutput());
	writer->SetInput(filter->GetOutput());

	filter->SetOutsideValue(0);
	filter->SetInsideValue(70);
	filter->SetLowerThreshold(70);
	filter->SetUpperThreshold(100);
	filter->Update();
	writer->SetFileName("images-ribs/IM50.dcm");

	writer->UseInputMetaDataDictionaryOff();
	writer->SetImageIO(gdcmImageIO);
	writer->Update();
	return 0;

}
 


