/***
 * millipede: FeatureIdentifier.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "FeatureIdentifier.h"

#include <boost/bind.hpp>

namespace mp {

//#################### CONSTRUCTORS ####################
FeatureIdentifier::FeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	m_dicomVolume(dicomVolume), m_volumeIPF(volumeIPF)
{
	m_mfsHook.set(VolumeIPFMultiFeatureSelection_Ptr(new VolumeIPFMultiFeatureSelectionT(volumeIPF)));
}

//#################### PUBLIC METHODS ####################
const DataHook<FeatureIdentifier::VolumeIPFMultiFeatureSelection_Ptr>& FeatureIdentifier::get_mfs_hook() const
{
	return m_mfsHook;
}

const FeatureIdentifier::VolumeIPFMultiFeatureSelection_Ptr& FeatureIdentifier::get_multi_feature_selection() const
{
	return m_mfsHook.get();
}

void FeatureIdentifier::set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook)
{
	m_mfsHook = mfsHook;
}

//#################### PROTECTED METHODS ####################
DICOMVolume_CPtr FeatureIdentifier::dicom_volume() const
{
	return m_dicomVolume;
}

void FeatureIdentifier::morphologically_close_nodes(std::set<PFNodeID>& nodes, int n) const
{
	morphologically_close_nodes(nodes, boost::bind(&FeatureIdentifier::morphological_condition_accept_all, this, _1));
}

void FeatureIdentifier::morphologically_dilate_nodes(std::set<PFNodeID>& nodes, int n) const
{
	morphologically_dilate_nodes(nodes, boost::bind(&FeatureIdentifier::morphological_condition_accept_all, this, _1));
}

void FeatureIdentifier::morphologically_erode_nodes(std::set<PFNodeID>& nodes, int n) const
{
	morphologically_erode_nodes(nodes, boost::bind(&FeatureIdentifier::morphological_condition_accept_all, this, _1));
}

FeatureIdentifier::VolumeIPF_Ptr FeatureIdentifier::volume_ipf() const
{
	return m_volumeIPF;
}

//#################### PRIVATE METHODS ####################
bool FeatureIdentifier::morphological_condition_accept_all(const BranchProperties&) const
{
	return true;
}

}
