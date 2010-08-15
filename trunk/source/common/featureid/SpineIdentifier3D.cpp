/***
 * millipede: SpineIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SpineIdentifier3D.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SpineIdentifier3D::SpineIdentifier3D(const VolumeIPF_Ptr& volumeIPF)
:	m_volumeIPF(volumeIPF)
{}

//#################### PUBLIC METHODS ####################
const SpineIdentifier3D::VolumeIPFMultiFeatureSelection_Ptr& SpineIdentifier3D::get_output() const
{
	return m_outputHook.get();
}

const DataHook<SpineIdentifier3D::VolumeIPFMultiFeatureSelection_Ptr>& SpineIdentifier3D::get_output_hook() const
{
	return m_outputHook;
}

int SpineIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void SpineIdentifier3D::execute_impl()
{
	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection(new VolumeIPFMultiFeatureSelectionT(m_volumeIPF));

	// TODO

	m_outputHook.set(multiFeatureSelection);
}

}
