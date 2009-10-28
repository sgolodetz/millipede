/***
 * millipede: PatientRecord.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PatientRecord.h"

#include <cassert>

#include <common/exceptions/Exception.h>
#include "StudyRecord.h"

namespace mp {

//#################### CONSTRUCTORS ####################
PatientRecord::PatientRecord(const std::string& patientsName)
:	m_patientsName(patientsName)
{}

//#################### PUBLIC METHODS ####################
void PatientRecord::add_study_record(const StudyRecord_CPtr& studyRecord)
{
	assert(studyRecord != NULL);
	bool succeeded = m_studyRecords.insert(std::make_pair(studyRecord->key(), studyRecord)).second;
	if(!succeeded) throw Exception("Study already exists: " + studyRecord->key());
}

std::string PatientRecord::key() const
{
	return m_patientsName;
}

const std::string& PatientRecord::patients_name() const
{
	return m_patientsName;
}

int PatientRecord::study_count() const
{
	return static_cast<int>(m_studyRecords.size());
}

const StudyRecord& PatientRecord::study_record(const std::string& studyKey) const
{
	std::map<std::string,StudyRecord_CPtr>::const_iterator it = m_studyRecords.find(studyKey);
	if(it != m_studyRecords.end()) return *it->second;
	else throw Exception("Study not found: " + studyKey);
}

const std::map<std::string,StudyRecord_CPtr>& PatientRecord::study_records() const
{
	return m_studyRecords;
}

}
