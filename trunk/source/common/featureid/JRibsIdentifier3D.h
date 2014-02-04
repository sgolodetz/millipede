/***
 * millipede: JRibsIdentifier3D.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_JRIBSIDENTIFIER3D
#define H_MILLIPEDE_JRIBSIDENTIFIER3D

#include <common/jobs/SimpleJob.h>
#include "FeatureIdentifier.h"
#include <string>
#include <common/featureid/FIOptions.h>

namespace mp {

class JRibsIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	JRibsIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, const FIOptions map);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	FIOptions m_map;
	
	void execute_impl();
	bool grow_condition(const PFNodeID& adj, const BranchProperties& adjProperties, const BranchProperties& curProperties, const BranchProperties& seedProperties, const BranchProperties& overallProperties, const FIOptions map) const;
	bool is_seed(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, const FIOptions map) const;
	PartitionForestSelection_Ptr postprocess_regions(const PartitionForestSelection_Ptr& preliminaryRegions, const BranchProperties& spineProperties, const FIOptions map) const;
	bool good_position(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, const FIOptions map) const;
};

}

#endif
