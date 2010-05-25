/***
 * millipede: PartitionModel.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionModel.h"

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionModel::PartitionModel(const Volume_Ptr& volume, const ViewLocation& loc, SliceOrientation ori)
:	m_sliceOrientation(ori), m_viewLocation(loc), m_volume(volume)
{}

//#################### PUBLIC METHODS ####################
void PartitionModel::add_listener(Listener *listener)
{
	m_listeners.push_back(listener);
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

void PartitionModel::set_volume_texture_set(const VolumeTextureSet_Ptr& textureSet)
{
	m_textureSet = textureSet;
	alert_listeners();
}

SliceOrientation PartitionModel::slice_orientation() const					{ return m_sliceOrientation; }
const PartitionModel::ViewLocation& PartitionModel::view_location() const	{ return m_viewLocation; }
const Volume_Ptr& PartitionModel::volume()									{ return m_volume; }
Volume_CPtr PartitionModel::volume() const									{ return m_volume; }
const VolumeTextureSet_Ptr& PartitionModel::volume_texture_set()			{ return m_textureSet; }
VolumeTextureSet_CPtr PartitionModel::volume_texture_set() const			{ return m_textureSet; }

//#################### PRIVATE METHODS ####################
void PartitionModel::alert_listeners()
{
	for(size_t i=0, size=m_listeners.size(); i<size; ++i)
	{
		m_listeners[i]->model_changed();
	}
}

}
