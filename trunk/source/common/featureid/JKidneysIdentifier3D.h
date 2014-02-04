/***
 * millipede: JKidneysIdentifier3D.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_JKIDNEYSIDENTIFIER3D
#define H_MILLIPEDE_JKIDNEYSIDENTIFIER3D

#include <common/jobs/SimpleJob.h>
#include "FeatureIdentifier.h"
#include <string>
#include <common/featureid/FIOptions.h>

namespace mp {

class JKidneysIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	JKidneysIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, const FIOptions map);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	FIOptions m_map;
	void execute_impl();
	bool is_kidneyL(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, const BranchProperties& ribProperties, const FIOptions map) const;
	bool is_kidneyR(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& spineProperties, const BranchProperties& ribProperties, const FIOptions map) const;
	
	bool grow_condition(const PFNodeID& adj, const BranchProperties& adjProperties, const BranchProperties& curProperties, const BranchProperties& seedProperties, const BranchProperties& overallProperties, const BranchProperties& spineProperties, const FIOptions map) const;
	
	bool touching_spine(const PFNodeID& node, VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection, VolumeIPF_Ptr volumeIPF) const;
};

}

#endif
