/***
 * millipede: SeriesRecord.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SERIESRECORD
#define H_MILLIPEDE_SERIESRECORD

#include <string>
#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

namespace mp {

class SeriesRecord
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<std::string> m_imageFilenames;
	std::string m_seriesNumber;

	//#################### CONSTRUCTORS ####################
public:
	explicit SeriesRecord(const std::string& seriesNumber);

	//#################### PUBLIC METHODS ####################
public:
	void add_image_filename(const std::string& imageFilename);
	int image_count() const;
	const std::vector<std::string>& image_filenames() const;
	int image_height() const;
	int image_width() const;
	std::string key() const;
	const std::string& series_number() const;
};

//#################### TYPEDEFS ####################
typedef shared_ptr<SeriesRecord> SeriesRecord_Ptr;
typedef shared_ptr<const SeriesRecord> SeriesRecord_CPtr;

}

#endif
