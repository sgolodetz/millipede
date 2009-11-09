/***
 * millipede: VolumeLoader.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeLoader.h"

#include <boost/thread/mutex.hpp>

#include <itkGDCMImageIO.h>
#include <itkImageFileReader.h>
#include <itkJoinSeriesImageFilter.h>

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
	const double sliceLocation = 0.0;	// FIXME
	joiner->SetOrigin(sliceLocation);
	const double sliceThickness = 1.0;	// FIXME
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

}
