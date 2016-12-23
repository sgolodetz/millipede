/***
 * millipede: StudyRecord.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_STUDYRECORD
#define H_MILLIPEDE_STUDYRECORD

#include <string>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include "../../adts/Map.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<const class SeriesRecord> SeriesRecord_CPtr;

class StudyRecord
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_studyDescription, m_studyID, m_studyInstanceUID;
	Map<std::string,SeriesRecord_CPtr> m_seriesRecords;

	//#################### CONSTRUCTORS ####################
public:
	StudyRecord(const std::string& studyDescription, const std::string& studyID, const std::string& studyInstanceUID);

	//#################### PUBLIC METHODS ####################
public:
	void add_series_record(const SeriesRecord_CPtr& seriesRecord);
	std::string key() const;
	int series_count() const;
	const SeriesRecord& series_record(const std::string& seriesKey) const;
	const Map<std::string,SeriesRecord_CPtr>& series_records() const;
};

//#################### TYPEDEFS ####################
typedef shared_ptr<StudyRecord> StudyRecord_Ptr;
typedef shared_ptr<const StudyRecord> StudyRecord_CPtr;

}

#endif
