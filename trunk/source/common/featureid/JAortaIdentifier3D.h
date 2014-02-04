/***
 * millipede: JAortaIdentifier3D.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_JAORTAIDENTIFIER3D
#define H_MILLIPEDE_JAORTAIDENTIFIER3D

#include <common/jobs/SimpleJob.h>
#include "FeatureIdentifier.h"
#include <string>
#include <common/featureid/FIOptions.h>

namespace mp {

class JAortaIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	JAortaIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, const FIOptions map);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	FIOptions m_map;
	void execute_impl();
	bool is_seed(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, const BranchProperties& spinalCordProperties, const FIOptions map) const;
	bool morphological_condition(const BranchProperties& properties, const FIOptions map) const;
};

}

#endif
