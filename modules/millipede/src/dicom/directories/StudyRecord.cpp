/***
 * millipede: StudyRecord.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "StudyRecord.h"

#include <cassert>
#include <sstream>

#include <millipede/exceptions/Exception.h>
#include "SeriesRecord.h"

namespace mp {

//#################### CONSTRUCTORS ####################
StudyRecord::StudyRecord(const std::string& studyDescription, const std::string& studyID, const std::string& studyInstanceUID)
:	m_studyDescription(studyDescription), m_studyID(studyID), m_studyInstanceUID(studyInstanceUID)
{}

//#################### PUBLIC METHODS ####################
void StudyRecord::add_series_record(const SeriesRecord_CPtr& seriesRecord)
{
	assert(seriesRecord != NULL);
	if(!m_seriesRecords.insert(seriesRecord->key(), seriesRecord))
	{
		throw Exception("Series already exists: " + seriesRecord->key());
	}
}

std::string StudyRecord::key() const
{
	std::ostringstream oss;
	oss << m_studyID << " (" << m_studyDescription << ", " << m_studyInstanceUID << ')';
	return oss.str();
}

int StudyRecord::series_count() const
{
	return static_cast<int>(m_seriesRecords.size());
}

const SeriesRecord& StudyRecord::series_record(const std::string& seriesKey) const
{
	optional<const SeriesRecord_CPtr&> ret = m_seriesRecords.get(seriesKey);
	if(ret) return **ret;
	else throw Exception("Series not found: " + seriesKey);
}

const Map<std::string,SeriesRecord_CPtr>& StudyRecord::series_records() const
{
	return m_seriesRecords;
}

}
