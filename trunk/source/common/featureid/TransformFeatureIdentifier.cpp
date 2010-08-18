/***
 * millipede: TransformFeatureIdentifier.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "TransformFeatureIdentifier.h"

namespace mp {

//#################### CONSTRUCTORS ####################
TransformFeatureIdentifier::TransformFeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	SingleOutputFeatureIdentifier(dicomVolume, volumeIPF)
{}

}
