/***
 * millipede: VolumeLoader.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeLoader.h"

#include <boost/algorithm/string/trim.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/thread/mutex.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <itkGDCMImageIO.h>
#include <itkImageFileReader.h>
#include <itkJoinSeriesImageFilter.h>
#include <itkMetaDataObject.h>

#include <common/dicom/directories/DICOMDirectory.h>
#include <common/dicom/volumes/Volume.h>
#include <common/exceptions/Exception.h>

namespace mp {

//#################### CONSTRUCTORS ####################
VolumeLoader::VolumeLoader(const DICOMDirectory_CPtr& dicomdir, const VolumeChoice& volumeChoice)
:	m_dicomdir(dicomdir), m_volumeChoice(volumeChoice), m_aborted(false)
{}

//#################### PUBLIC METHODS ####################
void VolumeLoader::abort()
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_aborted = true;
}

bool VolumeLoader::aborted() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_aborted;
}

void VolumeLoader::load()
try
{
	typedef itk::GDCMImageIO ImageIO;
	typedef itk::Image<signed int,2> Image2D;
	typedef itk::Image<signed int,3> Image3D;
	typedef itk::JoinSeriesImageFilter<Image2D,Image3D> Joiner;
	typedef itk::ImageFileReader<Image2D> Reader;

	std::vector<std::string> imageFilenames = m_dicomdir->image_filenames(m_volumeChoice.patientKey, m_volumeChoice.studyKey, m_volumeChoice.seriesKey);
	std::vector<Image2D::Pointer> images(imageFilenames.size());
	for(int i=m_volumeChoice.minZ; i<=m_volumeChoice.maxZ; ++i)
	{
		std::string imageFilename = m_volumeChoice.filePrefix + imageFilenames[i];

		// Introduce a local scope to make sure the mutex gets unlocked as soon as possible.
		{
			boost::mutex::scoped_lock lock(m_mutex);
			if(!m_aborted)
			{
				m_progress = i - m_volumeChoice.minZ;
				m_status = "Loading image " + imageFilename + "...";
			}
			else return;
		}

		// Load the image.
		Reader::Pointer reader = Reader::New();
		reader->SetFileName(imageFilename);
		ImageIO::Pointer gdcmImageIO = ImageIO::New();
		reader->SetImageIO(gdcmImageIO);
		reader->Update();
		images[i] = reader->GetOutput();
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
	m_volume.reset(new Volume(volumeImage));

	boost::mutex::scoped_lock lock(m_mutex);
	m_progress = max();
}
catch(Exception& e)
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_aborted = true;
	m_status = e.cause();
}
catch(itk::ExceptionObject& e)
{
	boost::mutex::scoped_lock lock(m_mutex);
	m_aborted = true;
	m_status = e.what();
}

int VolumeLoader::max() const
{
	return m_volumeChoice.maxZ - m_volumeChoice.minZ + 1;
}

int VolumeLoader::progress() const
{
	boost::mutex::scoped_lock lock(m_mutex);
	return m_progress;
}

std::string VolumeLoader::status() const
{
	return m_status;
}

const Volume_Ptr& VolumeLoader::volume()
{
	return m_volume;
}

//#################### PRIVATE METHODS ####################
std::string VolumeLoader::read_header_field(const itk::Image<signed int,2>::Pointer& image, const std::string& key)
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
