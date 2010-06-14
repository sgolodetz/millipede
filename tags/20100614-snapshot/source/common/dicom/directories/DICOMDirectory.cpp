/***
 * millipede: DICOMDirectory.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "DICOMDirectory.h"

#include <common/exceptions/Exception.h>
#include "PatientRecord.h"
#include "SeriesRecord.h"
#include "StudyRecord.h"

namespace mp {

//#################### PUBLIC METHODS ####################
void DICOMDirectory::add_patient_record(const PatientRecord_CPtr& patientRecord)
{
	m_patientRecords.push_back(patientRecord);
}

const std::vector<std::string>& DICOMDirectory::image_filenames(const std::string& patientKey, const std::string& studyKey, const std::string& seriesKey) const
{
	std::vector<PatientRecord_CPtr> pRecords = patient_records(patientKey);
	for(std::vector<PatientRecord_CPtr>::const_iterator it=pRecords.begin(), iend=pRecords.end(); it!=iend; ++it)
	{
		try
		{
			const StudyRecord& stRecord = (*it)->study_record(studyKey);
			const SeriesRecord& seRecord = stRecord.series_record(seriesKey);
			return seRecord.image_filenames();
		}
		catch(Exception&) {}
	}

	throw Exception("Study/Series not found: " + studyKey + "/" + seriesKey);
}

int DICOMDirectory::patient_count() const
{
	return static_cast<int>(m_patientRecords.size());
}

std::vector<PatientRecord_CPtr> DICOMDirectory::patient_records(const std::string& patientKey) const
{
	// Note: We have to be able to return multiple patient records here, since a single patient may (unhelpfully) have multiple records in the DICOMDIR.
	std::vector<PatientRecord_CPtr> ret;

	for(size_t i=0, size=m_patientRecords.size(); i<size; ++i)
	{
		if(m_patientRecords[i]->key() == patientKey) ret.push_back(m_patientRecords[i]);
	}

	if(ret.empty()) throw Exception("Patient not found: " + patientKey);

	return ret;
}

const std::vector<PatientRecord_CPtr>& DICOMDirectory::patient_records() const
{
	return m_patientRecords;
}

}
