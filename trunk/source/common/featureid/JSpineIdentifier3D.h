/***
 * millipede: JSpineIdentifier3D.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_JSPINEIDENTIFIER3D
#define H_MILLIPEDE_JSPINEIDENTIFIER3D

#include <common/jobs/SimpleJob.h>
#include "FeatureIdentifier.h"
#include <common/featureid/FIOptions.h>

namespace mp {

class JSpineIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	JSpineIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
	bool is_spine(const PFNodeID& node, const BranchProperties& properties) const;
};

}

#endif
