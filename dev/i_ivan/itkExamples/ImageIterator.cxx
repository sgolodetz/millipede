#include "itkImage.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkGDCMImageIO.h"

int main() {
	const unsigned int Dimension = 2;
	typedef signed short PixelType;
	typedef itk::Image<PixelType, Dimension> ImageType;
	typedef itk::ImageRegionConstIterator<ImageType> ConstantIteratorType;
	typedef itk::ImageRegionIterator<ImageType> IteratorType;
	typedef itk::ImageFileReader<ImageType> ReaderType;
	typedef itk::ImageFileWriter<ImageType> WriterType;
	typedef itk::GDCMImageIO ImageIOType;

	ImageIOType::Pointer gdcmImageIO = ImageIOType::New();
	//ImageType::RegionType inputRegion;
	ReaderType::Pointer reader = ReaderType::New();
	reader->SetFileName("images/IM50");
	reader->SetImageIO(gdcmImageIO);

	try {
		reader->Update();
	} catch ( itk::ExceptionObject &err) {
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << err << std::endl;
		return -1;
	}


	ImageType::Pointer outputImage = ImageType::New();
	outputImage->SetRegions(reader->GetOutput()->GetLargestPossibleRegion());
	outputImage->Allocate();



	ConstantIteratorType iterator(reader->GetOutput(), reader->GetOutput()->GetLargestPossibleRegion());
	IteratorType outputIt(outputImage, reader->GetOutput()->GetLargestPossibleRegion());


	for (iterator.GoToBegin(), outputIt.GoToBegin(); !iterator.IsAtEnd();
			++iterator, ++outputIt) {
		if (iterator.Get() > 70 && iterator.Get() < 100) {
			outputIt.Set(70);
		} else {
			outputIt.Set(0);
		}
	}

	WriterType::Pointer writer = WriterType::New();
	writer->SetFileName("images-results/IM50-iterator.dcm");
	writer->SetInput(outputImage);

	writer->UseInputMetaDataDictionaryOff();
	writer->SetImageIO(gdcmImageIO);

	try {
		writer->Update();
	} catch ( itk::ExceptionObject &err) {
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << err << std::endl;
		return -1;
	}
	return 0;
}
