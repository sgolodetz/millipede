/***
 * millipede: SeriesRecord.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "SeriesRecord.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SeriesRecord::SeriesRecord(const std::string& seriesNumber)
:	m_seriesNumber(seriesNumber)
{}

//#################### PUBLIC METHODS ####################
void SeriesRecord::add_image_filename(const std::string& imageFilename)
{
	m_imageFilenames.push_back(imageFilename);
}

int SeriesRecord::image_count() const
{
	return static_cast<int>(m_imageFilenames.size());
}

const std::vector<std::string>& SeriesRecord::image_filenames() const
{
	return m_imageFilenames;
}

int SeriesRecord::image_height() const
{
	// FIXME: This should be loaded in from the image header.
	return 512;
}

int SeriesRecord::image_width() const
{
	// FIXME: This should be loaded in from the image header.
	return 512;
}

std::string SeriesRecord::key() const
{
	return m_seriesNumber;
}

const std::string& SeriesRecord::series_number() const
{
	return m_seriesNumber;
}

}
