/***
 * millipede: SpineIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SpineIdentifier3D.h"

#include <common/dicom/volumes/DICOMVolume.h>

namespace mp {

//#################### CONSTRUCTORS ####################
SpineIdentifier3D::SpineIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	m_dicomVolume(dicomVolume), m_volumeIPF(volumeIPF)
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

	PFNodeID seed = find_seed();
	if(seed != PFNodeID::invalid())
	{
		multiFeatureSelection->identify_node(seed, AbdominalFeature::VERTEBRA);
	}

	m_outputHook.set(multiFeatureSelection);
}

PFNodeID SpineIdentifier3D::find_seed() const
{
	itk::Size<3> volumeSize = m_dicomVolume->size();
	int minSpineVoxels = 4000 * volumeSize[2];
	int maxSpineVoxels = 8000 * volumeSize[2];
	for(int layer=1, highestLayer=m_volumeIPF->highest_layer(); layer<=highestLayer; ++layer)
	{
		for(BranchNodeConstIterator it=m_volumeIPF->branch_nodes_cbegin(layer), iend=m_volumeIPF->branch_nodes_cend(layer); it!=iend; ++it)
		{
			const BranchProperties& properties = it->properties();
			if(	properties.z_min() == 0 && properties.z_max() == volumeSize[2]-1 &&		// the spine should extend through all the slices we're looking at
				properties.mean_grey_value() >= 180 &&									// it should have a reasonably white grey value
				properties.voxel_count() >= minSpineVoxels &&							// and a reasonable size
				properties.voxel_count() <= maxSpineVoxels)
			{
				return PFNodeID(layer, it.index());
			}
		}
	}

	return PFNodeID();
}

}
