/***
 * millipede: PatientRecord.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PatientRecord.h"

#include <cassert>

#include <millipede/exceptions/Exception.h>
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
	if(!m_studyRecords.insert(studyRecord->key(), studyRecord))
	{
		throw Exception("Study already exists: " + studyRecord->key());
	}
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
	optional<const StudyRecord_CPtr&> ret = m_studyRecords.get(studyKey);
	if(ret) return **ret;
	else throw Exception("Study not found: " + studyKey);
}

const Map<std::string,StudyRecord_CPtr>& PatientRecord::study_records() const
{
	return m_studyRecords;
}

}
