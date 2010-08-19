/***
 * millipede: FeatureIdentifier.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "FeatureIdentifier.h"

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
	morphologically_dilate_nodes(nodes, n);
	morphologically_erode_nodes(nodes, n);
}

void FeatureIdentifier::morphologically_dilate_nodes(std::set<PFNodeID>& nodes, int n) const
{
	for(int k=0; k<n; ++k)
	{
		std::set<PFNodeID> initialNodes = nodes;
		for(std::set<PFNodeID>::const_iterator it=initialNodes.begin(), iend=initialNodes.end(); it!=iend; ++it)
		{
			std::vector<int> adjNodes = volume_ipf()->adjacent_nodes(*it);
			for(std::vector<int>::const_iterator jt=adjNodes.begin(), jend=adjNodes.end(); jt!=jend; ++jt)
			{
				nodes.insert(PFNodeID(it->layer(), *jt));
			}
		}
	}
}

void FeatureIdentifier::morphologically_erode_nodes(std::set<PFNodeID>& nodes, int n) const
{
	for(int k=0; k<n; ++k)
	{
		std::set<PFNodeID> initialNodes = nodes;
		for(std::set<PFNodeID>::const_iterator it=initialNodes.begin(), iend=initialNodes.end(); it!=iend; ++it)
		{
			std::vector<int> adjNodes = volume_ipf()->adjacent_nodes(*it);
			for(std::vector<int>::const_iterator jt=adjNodes.begin(), jend=adjNodes.end(); jt!=jend; ++jt)
			{
				PFNodeID adjNode(it->layer(), *jt);
				if(initialNodes.find(adjNode) == initialNodes.end())	// *it has an adjacent node that wasn't marked initially
				{
					nodes.erase(*it);
					break;
				}
			}
		}
	}
}

FeatureIdentifier::VolumeIPF_Ptr FeatureIdentifier::volume_ipf() const
{
	return m_volumeIPF;
}

}
