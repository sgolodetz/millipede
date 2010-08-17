/***
 * millipede: SpineIdentifier3D.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SpineIdentifier3D.h"

#include <boost/bind.hpp>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/util/ITKImageUtil.h>
#include "FeatureIdentificationUtil.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SpineIdentifier3D::SpineIdentifier3D(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF)
:	m_dicomVolume(dicomVolume), m_volumeIPF(volumeIPF)
{}

//#################### PUBLIC METHODS ####################
const DataHook<SpineIdentifier3D::VolumeIPFMultiFeatureSelection_Ptr>& SpineIdentifier3D::get_mfs_hook() const
{
	return m_mfsHook;
}

const SpineIdentifier3D::VolumeIPFMultiFeatureSelection_Ptr& SpineIdentifier3D::get_output() const
{
	return m_mfsHook.get();
}

int SpineIdentifier3D::length() const
{
	return 1;
}

//#################### PRIVATE METHODS ####################
void SpineIdentifier3D::execute_impl()
{
	set_status("Identifying the spine...");

	VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection(new VolumeIPFMultiFeatureSelectionT(m_volumeIPF));

	std::list<PFNodeID> nodes = FeatureIdentificationUtil::filter_branch_nodes(m_volumeIPF, boost::bind(&SpineIdentifier3D::is_spine, this, _1));
	for(std::list<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		multiFeatureSelection->identify_node(*it, AbdominalFeature::VERTEBRA);
	}

	m_mfsHook.set(multiFeatureSelection);
}

bool SpineIdentifier3D::is_spine(const BranchProperties& properties) const
{
	itk::Index<3> volumeSize = ITKImageUtil::make_index_from_size(m_dicomVolume->size());
	int minVoxels = 800 * volumeSize[2];
	int maxVoxels = 8000 * volumeSize[2];
	double aspectRatio = properties.aspect_ratio();

	return	properties.x_min() < volumeSize[0]/2 && properties.x_max() > volumeSize[0]/2 &&		// it should straddle x = volumeSize[0] / 2
			properties.y_max() > volumeSize[1]/2 &&												// its base should be below y = volumeSize[1]/2
			properties.z_min() == 0 && properties.z_max() == volumeSize[2]-1 &&					// it should extend through all the slices we're looking at
			0.25 <= aspectRatio && aspectRatio <= 4 &&											// it should have a reasonable aspect ratio
			properties.mean_grey_value() >= 180 &&												// it should have a reasonably high grey value
			properties.voxel_count() >= minVoxels && properties.voxel_count() <= maxVoxels;		// and a reasonable size
}

}
