/***
 * millipede: JSpinalCordIdentifier3D.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_JSPINALCORDIDENTIFIER3D
#define H_MILLIPEDE_JSPINALCORDIDENTIFIER3D

#include <common/jobs/SimpleJob.h>
#include "FeatureIdentifier.h"
#include <common/featureid/FIOptions.h>

namespace mp {

class JSpinalCordIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	JSpinalCordIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
	bool is_spinal_cord(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties) const;
};

}

#endif
