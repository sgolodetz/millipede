/***
 * millipede: DICOMDirectory.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMDIRECTORY
#define H_MILLIPEDE_DICOMDIRECTORY

#include <string>
#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<const class PatientRecord> PatientRecord_CPtr;

class DICOMDirectory
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<PatientRecord_CPtr> m_patientRecords;

	//#################### PUBLIC METHODS ####################
public:
	void add_patient_record(const PatientRecord_CPtr& patientRecord);
	const std::vector<std::string>& image_filenames(const std::string& patientKey, const std::string& studyKey, const std::string& seriesKey) const;
	int patient_count() const;
	std::vector<PatientRecord_CPtr> patient_records(const std::string& patientKey) const;
	const std::vector<PatientRecord_CPtr>& patient_records() const;
};

//#################### TYPEDEFS ####################
typedef shared_ptr<DICOMDirectory> DICOMDirectory_Ptr;
typedef shared_ptr<const DICOMDirectory> DICOMDirectory_CPtr;

}

#endif
