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
#include <common/partitionforests/images/ImageBranchLayer.h>
#include <common/partitionforests/images/ImageLeafLayer.h>
#include <common/segmentation/watershed/MeijsterRoerdinkWatershed.h>
using namespace mp;

template <typename PixelType>
typename itk::Image<PixelType,2>::Pointer create_2d_image(const PixelType *const pixels, int width, int height)
{
	typedef itk::Image<PixelType,2> Image;
	typedef typename Image::Pointer ImagePointer;
	typedef typename Image::IndexType Index;
	typedef typename Image::RegionType Region;
	typedef typename Image::SizeType Size;

	Index start;
	start.Fill(0);
	Size size;
	size[0] = width;
	size[1] = height;
	Region region;
	region.SetIndex(start);
	region.SetSize(size);
	ImagePointer image = Image::New();
	image->SetRegions(region);
	image->Allocate();

	const PixelType *p = pixels;
	for(int y=0; y<height; ++y)
	{
		for(int x=0; x<width; ++x)
		{
			Index index;
			index[0] = x;
			index[1] = y;
			image->SetPixel(index, *p++);
		}
	}

	return image;
}

template <typename PixelType>
typename itk::Image<PixelType,3>::Pointer create_3d_image(const PixelType *const pixels, int sizeX, int sizeY, int sizeZ)
{
	typedef itk::Image<PixelType,3> Image;
	typedef typename Image::Pointer ImagePointer;
	typedef typename Image::IndexType Index;
	typedef typename Image::RegionType Region;
	typedef typename Image::SizeType Size;

	Index start;
	start.Fill(0);
	Size size;
	size[0] = sizeX;
	size[1] = sizeY;
	size[2] = sizeZ;
	Region region;
	region.SetIndex(start);
	region.SetSize(size);
	ImagePointer image = Image::New();
	image->SetRegions(region);
	image->Allocate();

	const PixelType *p = pixels;
	for(int z=0; z<sizeZ; ++z)
		for(int y=0; y<sizeY; ++y)
			for(int x=0; x<sizeX; ++x)
			{
				Index index;
				index[0] = x;
				index[1] = y;
				index[2] = z;
				image->SetPixel(index, *p++);
			}

	return image;
}

template <typename ImagePointer>
void output_2d_image(std::ostream& os, const ImagePointer& image)
{
	// FIXME: This is a hacky bit of test code - not for production use.
	typedef typename ImagePointer::ObjectType Image;
	typedef typename Image::IndexType Index;
	typedef typename Image::SizeType Size;

	assert(Size::GetSizeDimension() == 2);

	const Size& size = image->GetLargestPossibleRegion().GetSize();
	int width = size[0], height = size[1];

	for(int y=0; y<height; ++y)
	{
		for(int x=0; x<width; ++x)
		{
			Index index;
			index[0] = x;
			index[1] = y;
			os << image->GetPixel(index) << '\t';
		}
		os << '\n';
	}
}

template <typename ImagePointer>
void output_3d_image(std::ostream& os, const ImagePointer& image)
{
	// FIXME: This is a hacky bit of test code - not for production use.
	typedef typename ImagePointer::ObjectType Image;
	typedef typename Image::IndexType Index;
	typedef typename Image::SizeType Size;

	assert(Size::GetSizeDimension() == 3);

	const Size& size = image->GetLargestPossibleRegion().GetSize();
	int sizeX = size[0], sizeY = size[1], sizeZ = size[2];

	for(int z=0; z<sizeZ; ++z)
	{
		if(z > 0) os << "---\n";
		for(int y=0; y<sizeY; ++y)
		{
			for(int x=0; x<sizeX; ++x)
			{
				Index index;
				index[0] = x;
				index[1] = y;
				index[2] = z;
				os << image->GetPixel(index) << '\t';
			}
			os << '\n';
		}
	}
}

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

	Image::Pointer image = create_2d_image(pixels, 7, 5);
	output_2d_image(std::cout, image);

	typedef MeijsterRoerdinkWatershed<int,2> WS;

	// Specify the necessary offsets for 4-connectivity.
	WS::NeighbourOffsets offsets(4);
	offsets[0][0] = 0;		offsets[0][1] = -1;		// above
	offsets[1][0] = -1;		offsets[1][1] = 0;		// left
	offsets[2][0] = 1;		offsets[2][1] = 0;		// right
	offsets[3][0] = 0;		offsets[3][1] = 1;		// below

	// Run the watershed algorithm on the image.
	WS ws(image, offsets);

	// Output the results.
	std::cout << '\n';
	output_2d_image(std::cout, ws.lower_complete());

	std::cout << '\n';
	output_2d_image(std::cout, ws.arrows());

	std::cout << '\n';
	output_2d_image(std::cout, ws.labels());
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

	Image::Pointer image = create_2d_image(pixels, 8, 8);
	output_2d_image(std::cout, image);
	std::cout << '\n';

	typedef itk::GradientMagnitudeImageFilter<Image,Image> GradientMagnitudeFilter;
	GradientMagnitudeFilter::Pointer gradientMagnitudeFilter = GradientMagnitudeFilter::New();
	gradientMagnitudeFilter->SetInput(image);
	gradientMagnitudeFilter->SetUseImageSpacingOff();
	gradientMagnitudeFilter->Update();

	Image::Pointer gradientMagnitudeImage = gradientMagnitudeFilter->GetOutput();
	output_2d_image(std::cout, gradientMagnitudeImage);
	std::cout << '\n';

	typedef MeijsterRoerdinkWatershed<int,2> WS;

	// Specify the necessary offsets for 4-connectivity.
	WS::NeighbourOffsets offsets(4);
	offsets[0][0] = 0;		offsets[0][1] = -1;		// above
	offsets[1][0] = -1;		offsets[1][1] = 0;		// left
	offsets[2][0] = 1;		offsets[2][1] = 0;		// right
	offsets[3][0] = 0;		offsets[3][1] = 1;		// below

	// Run the watershed algorithm on the gradient magnitude image.
	WS ws(gradientMagnitudeImage, offsets);

	// Output the results.
	output_2d_image(std::cout, ws.lower_complete());
	std::cout << '\n';

	output_2d_image(std::cout, ws.arrows());
	std::cout << '\n';

	output_2d_image(std::cout, ws.labels());
	std::cout << '\n';
}

void forest_test()
{
	// Create the Hounsfield and 'windowed' images.
	typedef itk::Image<int,3> HounsfieldImage;
	typedef itk::Image<unsigned char,3> WindowedImage;
	typedef itk::Image<int,3> GradientMagnitudeImage;

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

	HounsfieldImage::Pointer hounsfieldImage = create_3d_image(pixels, 8, 8, 1);
	output_3d_image(std::cout, hounsfieldImage);
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
	output_3d_image(std::cout, gradientMagnitudeImage);
	std::cout << '\n';

	typedef MeijsterRoerdinkWatershed<int,3> WS;

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

	// Output the results.
	output_3d_image(std::cout, ws.lower_complete());
	std::cout << '\n';

	output_3d_image(std::cout, ws.arrows());
	std::cout << '\n';

	output_3d_image(std::cout, ws.labels());
	std::cout << '\n';

	// Create the partition forest.
	typedef PartitionForest<ImageLeafLayer,ImageBranchLayer> IPF;
	typedef shared_ptr<IPF> IPF_Ptr;
	shared_ptr<ImageLeafLayer> leafLayer(new ImageLeafLayer(hounsfieldImage, windowedImage));
	shared_ptr<ImageBranchLayer> lowestBranchLayer = IPF::construct_lowest_branch_layer(leafLayer, ws.calculate_groups());
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

	// Cast the image to make its pixels real-valued.
	typedef itk::Image<float,2> RealImage;
	typedef itk::CastImageFilter<UCImage,RealImage> CastFilter;
	CastFilter::Pointer castFilter = CastFilter::New();
	castFilter->SetInput(reader->GetOutput());

	// Smooth the image using anisotropic diffusion.
	typedef itk::GradientAnisotropicDiffusionImageFilter<RealImage,RealImage> AnisotropicDiffusionFilter;
	AnisotropicDiffusionFilter::Pointer adFilter = AnisotropicDiffusionFilter::New();
	adFilter->SetInput(castFilter->GetOutput());
	adFilter->SetConductanceParameter(1.0);
	adFilter->SetNumberOfIterations(5);		// a typical value (see the ITK software guide)
	adFilter->SetTimeStep(0.125);

	// Calculate the gradient magnitude image.
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
	WS ws(gradientMagnitudeImage, offsets);

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
