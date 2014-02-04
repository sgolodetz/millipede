/***
 * millipede: JMultiFeatureIdentifier3D.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_JMULTIFEATUREIDENTIFIER3D
#define H_MILLIPEDE_JMULTIFEATUREIDENTIFIER3D

#include <common/jobs/CompositeJob.h>
#include "FeatureIdentifier.h"
#include <string>
#include <common/featureid/FIOptions.h>

namespace mp {

class JMultiFeatureIdentifier3D : public CompositeJob, public FeatureIdentifier
{
	//#################### CONSTRUCTORS ####################
public:
	JMultiFeatureIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, const FIOptions map);
};

}

#endif
