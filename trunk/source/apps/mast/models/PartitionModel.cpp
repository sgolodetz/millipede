/***
 * millipede: PartitionModel.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionModel.h"

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionModel::PartitionModel(const DICOMVolume_Ptr& dicomVolume, const ViewLocation& loc, SliceOrientation ori)
:	m_dicomVolume(dicomVolume), m_sliceOrientation(ori), m_viewLocation(loc)
{}

//#################### PUBLIC METHODS ####################
void PartitionModel::add_listener(Listener *listener)
{
	m_listeners.push_back(listener);
}

const VolumeTextureSet_Ptr& PartitionModel::dicom_texture_set()				{ return m_dicomTextureSet; }
VolumeTextureSet_CPtr PartitionModel::dicom_texture_set() const				{ return m_dicomTextureSet; }
const DICOMVolume_Ptr& PartitionModel::dicom_volume()						{ return m_dicomVolume; }
DICOMVolume_CPtr PartitionModel::dicom_volume() const						{ return m_dicomVolume; }

void PartitionModel::set_dicom_texture_set(const VolumeTextureSet_Ptr& dicomTextureSet)
{
	m_dicomTextureSet = dicomTextureSet;
	alert_listeners();
}

void PartitionModel::set_slice_orientation(SliceOrientation ori)
{
	m_sliceOrientation = ori;
	alert_listeners();
}

void PartitionModel::set_view_location(const ViewLocation& loc)
{
	// TODO: Validate location against bounds
	m_viewLocation = loc;
	alert_listeners();
}

SliceOrientation PartitionModel::slice_orientation() const					{ return m_sliceOrientation; }
const PartitionModel::ViewLocation& PartitionModel::view_location() const	{ return m_viewLocation; }

//#################### PRIVATE METHODS ####################
void PartitionModel::alert_listeners()
{
	for(size_t i=0, size=m_listeners.size(); i<size; ++i)
	{
		m_listeners[i]->model_changed();
	}
}

}
