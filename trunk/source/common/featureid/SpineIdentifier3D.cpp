/***
 * millipede: SpineIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SpineIdentifier3D.h"

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/util/ITKImageUtil.h>

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

	std::list<PFNodeID> seeds = find_seeds();

	// TEMPORARY
	for(std::list<PFNodeID>::const_iterator it=seeds.begin(), iend=seeds.end(); it!=iend; ++it)
	{
		multiFeatureSelection->identify_node(*it, AbdominalFeature::VERTEBRA);
	}

	m_outputHook.set(multiFeatureSelection);
}

std::list<PFNodeID> SpineIdentifier3D::find_seeds() const
{
	std::list<PFNodeID> seeds;

	itk::Index<3> volumeSize = ITKImageUtil::make_index_from_size(m_dicomVolume->size());
	int minSpineVoxels = 800 * volumeSize[2];
	int maxSpineVoxels = 8000 * volumeSize[2];
	for(int layer=1, highestLayer=m_volumeIPF->highest_layer(); layer<=highestLayer; ++layer)
	{
		for(BranchNodeConstIterator it=m_volumeIPF->branch_nodes_cbegin(layer), iend=m_volumeIPF->branch_nodes_cend(layer); it!=iend; ++it)
		{
			const BranchProperties& properties = it->properties();
			if(	properties.x_min() < volumeSize[0]/2 && properties.x_max() > volumeSize[0]/2 &&		// it should straddle x = volumeSize[0] / 2
				properties.y_max() > volumeSize[1]/2 &&												// its base should be below y = volumeSize[1]/2
				properties.z_min() == 0 && properties.z_max() == volumeSize[2]-1 &&					// it should extend through all the slices we're looking at
				properties.mean_grey_value() >= 180 &&												// it should have a reasonably white grey value
				properties.voxel_count() >= minSpineVoxels &&										// and a reasonable size
				properties.voxel_count() <= maxSpineVoxels)
			{
				seeds.push_back(PFNodeID(layer, it.index()));
			}
		}
	}

	return seeds;
}

}
