/***
 * millipede: VolumeLoader.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeLoader.h"

#include <boost/algorithm/string/trim.hpp>
#include <boost/lexical_cast.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <itkGDCMImageIO.h>
#include <itkImageFileReader.h>
#include <itkJoinSeriesImageFilter.h>
#include <itkMetaDataObject.h>
#include <itkRegionOfInterestImageFilter.h>

#include <common/dicom/directories/DICOMDirectory.h>
#include <common/dicom/volumes/DICOMVolume.h>
#include <common/exceptions/Exception.h>

namespace mp {

//#################### CONSTRUCTORS ####################
VolumeLoader::VolumeLoader(const DICOMDirectory_CPtr& dicomdir, const VolumeChoice& volumeChoice)
:	m_dicomdir(dicomdir), m_volumeChoice(volumeChoice)
{}

//#################### PUBLIC METHODS ####################
void VolumeLoader::execute()
try
{
	typedef itk::GDCMImageIO ImageIO;
	typedef itk::Image<int,2> Image2D;
	typedef itk::Image<int,3> Image3D;
	typedef itk::JoinSeriesImageFilter<Image2D,Image3D> Joiner;
	typedef itk::ImageFileReader<Image2D> Reader;
	typedef itk::RegionOfInterestImageFilter<Image2D,Image2D> RegionExtractor;

	// Set up the desired region for each of the slices.
	Image2D::RegionType region;
	Image2D::IndexType index;
	index[0] = m_volumeChoice.minX;
	index[1] = m_volumeChoice.minY;
	Image2D::SizeType size;
	size[0] = m_volumeChoice.maxX + 1 - m_volumeChoice.minX;
	size[1] = m_volumeChoice.maxY + 1 - m_volumeChoice.minY;
	region.SetIndex(index);
	region.SetSize(size);

	std::vector<std::string> imageFilenames = m_dicomdir->image_filenames(m_volumeChoice.patientKey, m_volumeChoice.studyKey, m_volumeChoice.seriesKey);
	std::vector<Image2D::Pointer> images(imageFilenames.size());
	for(int i=m_volumeChoice.minZ; i<=m_volumeChoice.maxZ; ++i)
	{
		std::string imageFilename = m_volumeChoice.filePrefix + imageFilenames[i];

		if(!is_aborted())
		{
			set_progress(i - m_volumeChoice.minZ);
			set_status("Loading image " + imageFilename + "...");
		}
		else return;

		// Load the image.
		Reader::Pointer reader = Reader::New();
		reader->SetFileName(imageFilename);
		ImageIO::Pointer gdcmImageIO = ImageIO::New();
		reader->SetImageIO(gdcmImageIO);
		reader->Update();

		// Extract the relevant sub-region.
		RegionExtractor::Pointer extractor = RegionExtractor::New();
		extractor->SetInput(reader->GetOutput());
		extractor->SetRegionOfInterest(region);
		extractor->Update();

		// Store the image.
		images[i] = extractor->GetOutput();
		images[i]->SetMetaDataDictionary(reader->GetMetaDataDictionary());
	}

	// Get the window centre and width if they haven't been explicitly specified by the user.
	if(m_volumeChoice.windowSettings.unspecified())
	{
		std::string windowCentreStr = read_header_field(images[m_volumeChoice.minZ], "0028|1050");
		std::string windowWidthStr = read_header_field(images[m_volumeChoice.minZ], "0028|1051");
		std::string validChars = "-0123456789";
		windowCentreStr = windowCentreStr.substr(0, windowCentreStr.find_first_not_of(validChars));
		windowWidthStr = windowWidthStr.substr(0, windowWidthStr.find_first_not_of(validChars));
		double windowCentre = lexical_cast<double>(windowCentreStr);
		double windowWidth = lexical_cast<double>(windowWidthStr);
		m_volumeChoice.windowSettings = WindowSettings(windowCentre, windowWidth);
	}

	// Make the images into a volume.
	Joiner::Pointer joiner = Joiner::New();

	double minSliceLocation = 0;
	try							{ minSliceLocation = lexical_cast<double>(read_header_field(images[m_volumeChoice.minZ], "0020|1041")); }
	catch(bad_lexical_cast&)	{ throw Exception("The SliceLocation value for the slice was not of the appropriate type"); }
	joiner->SetOrigin(minSliceLocation);

	double sliceThickness = 0;
	if(m_volumeChoice.minZ + 1 <= m_volumeChoice.maxZ)
	{
		try
		{
			double nextSliceLocation = lexical_cast<double>(read_header_field(images[m_volumeChoice.minZ+1], "0020|1041"));
			sliceThickness = fabs(nextSliceLocation - minSliceLocation);
		}
		catch(bad_lexical_cast&)	{}
	}
	if(sliceThickness == 0)
	{
		try							{ sliceThickness = lexical_cast<double>(read_header_field(images[m_volumeChoice.minZ], "0018|0050")); }
		catch(bad_lexical_cast&)	{ throw Exception("The SliceThickness value for the slice was not of the appropriate type"); }
	}
	joiner->SetSpacing(sliceThickness);

	for(int i=m_volumeChoice.minZ; i<=m_volumeChoice.maxZ; ++i) joiner->PushBackInput(images[i]);
	joiner->Update();

	Image3D::Pointer volumeImage = joiner->GetOutput();
	m_volume.reset(new DICOMVolume(volumeImage));

	set_finished();
}
catch(std::exception& e)
{
	abort();
	set_status(e.what());
}

int VolumeLoader::length() const
{
	return m_volumeChoice.maxZ - m_volumeChoice.minZ + 1;
}

const DICOMVolume_Ptr& VolumeLoader::volume()
{
	return m_volume;
}

const VolumeChoice& VolumeLoader::volume_choice() const
{
	return m_volumeChoice;
}

//#################### PRIVATE METHODS ####################
std::string VolumeLoader::read_header_field(const itk::Image<int,2>::Pointer& image, const std::string& key)
{
	const itk::MetaDataDictionary& dict = image->GetMetaDataDictionary();

	const itk::MetaDataObjectBase *baseVal = dict[key];
	if(!baseVal) throw Exception("No such key in image header: " + key);

	const itk::MetaDataObject<std::string> *strVal = dynamic_cast<const itk::MetaDataObject<std::string>*>(baseVal);
	if(!strVal) throw Exception("The value associated with " + key + " isn't a string");

	std::string ret = strVal->GetMetaDataObjectValue();
	boost::trim(ret);
	return ret;
}

}
