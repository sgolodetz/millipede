/***
 * millipede: LiverIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LiverIdentifier3D.h"

namespace mp {

//#################### CONSTRUCTORS ####################
LiverIdentifier3D::LiverIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	FeatureIdentifier(dicomVolume, volumeIPF)
{}

//#################### PUBLIC METHODS ####################
int LiverIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void LiverIdentifier3D::execute_impl()
{
	// TODO
}

}
