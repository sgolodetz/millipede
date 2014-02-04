/***
 * millipede: JLiverIdentifier3D.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_JLIVERIDENTIFIER3D
#define H_MILLIPEDE_JLIVERIDENTIFIER3D

#include <common/jobs/SimpleJob.h>
#include "FeatureIdentifier.h"
#include <string>
#include <common/featureid/FIOptions.h>

namespace mp {

class JLiverIdentifier3D : public SimpleJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	JLiverIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, const FIOptions map);

	//#################### PUBLIC METHODS ####################
public:
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	FIOptions m_map;
	
	void execute_impl();
	bool grow_condition(const PFNodeID& adj, const BranchProperties& adjProperties, const BranchProperties& curProperties, const BranchProperties& seedProperties, const BranchProperties& overallProperties, const FIOptions map) const;
	bool is_candidate(const PFNodeID& node, const BranchProperties& properties, const BranchProperties& ribProperties, const FIOptions map) const;
	bool morphological_condition(const BranchProperties& properties, const FIOptions map) const;
};

}

#endif
