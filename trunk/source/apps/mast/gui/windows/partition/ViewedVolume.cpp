/***
 * millipede: ViewedVolume.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "ViewedVolume.h"

#include "ViewedVolumeListener.h"

namespace mp {

//#################### CONSTRUCTORS ####################
ViewedVolume::ViewedVolume(const Volume_Ptr& volume, const ViewLocation& loc, SliceOrientation ori)
:	m_sliceOrientation(ori), m_viewLocation(loc), m_volume(volume)
{}

//#################### PUBLIC METHODS ####################
void ViewedVolume::add_listener(ViewedVolumeListener *listener)
{
	m_listeners.push_back(listener);
}

void ViewedVolume::set_slice_orientation(SliceOrientation ori)
{
	m_sliceOrientation = ori;
	alert_listeners();
}

void ViewedVolume::set_view_location(const ViewLocation& loc)
{
	// TODO: Validate location against bounds
	m_viewLocation = loc;
	alert_listeners();
}

void ViewedVolume::set_volume_texture_set(const VolumeTextureSet_Ptr& textureSet)
{
	m_textureSet = textureSet;
	alert_listeners();
}

SliceOrientation ViewedVolume::slice_orientation() const				{ return m_sliceOrientation; }
const ViewedVolume::ViewLocation& ViewedVolume::view_location() const	{ return m_viewLocation; }
const Volume_Ptr& ViewedVolume::volume()								{ return m_volume; }
Volume_CPtr ViewedVolume::volume() const								{ return m_volume; }
const VolumeTextureSet_Ptr& ViewedVolume::volume_texture_set()			{ return m_textureSet; }
VolumeTextureSet_CPtr ViewedVolume::volume_texture_set() const			{ return m_textureSet; }

//#################### PRIVATE METHODS ####################
void ViewedVolume::alert_listeners()
{
	for(size_t i=0, size=m_listeners.size(); i<size; ++i)
	{
		m_listeners[i]->viewed_volume_changed();
	}
}

}
