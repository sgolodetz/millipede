/***
 * millipede: PatientRecord.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PATIENTRECORD
#define H_MILLIPEDE_PATIENTRECORD

#include <string>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include "../../adts/Map.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<const class StudyRecord> StudyRecord_CPtr;

class PatientRecord
{
	//#################### PRIVATE METHODS ####################
private:
	std::string m_patientsName;
	Map<std::string,StudyRecord_CPtr> m_studyRecords;

	//#################### CONSTRUCTORS ####################
public:
	explicit PatientRecord(const std::string& patientsName);

	//#################### PUBLIC METHODS ####################
public:
	void add_study_record(const StudyRecord_CPtr& studyRecord);
	std::string key() const;
	const std::string& patients_name() const;
	int study_count() const;
	const StudyRecord& study_record(const std::string& studyKey) const;
	const Map<std::string,StudyRecord_CPtr>& study_records() const;
};

//#################### TYPEDEFS ####################
typedef shared_ptr<PatientRecord> PatientRecord_Ptr;
typedef shared_ptr<const PatientRecord> PatientRecord_CPtr;

}

#endif
