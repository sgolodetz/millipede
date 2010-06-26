/***
 * millipede: PartitionCamera.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "PartitionCamera.h"

#include <boost/optional.hpp>

#include <common/exceptions/Exception.h>
#include <common/util/ITKImageUtil.h>

namespace {

//#################### LOCAL CONSTANTS ####################
int PANNING_SPEED = 10;

}

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionCamera::PartitionCamera(const SliceLocation& sliceLocation, SliceOrientation sliceOrientation, const itk::Size<3>& volumeSize)
:	m_highestLayer(0),
	m_sliceLocation(sliceLocation),
	m_sliceOrientation(sliceOrientation),
	m_volumeSize(volumeSize),
	m_zoomLevel(0)
{
	if(!check_slice_location(m_sliceLocation)) throw Exception("Bad slice location");
}

//#################### PUBLIC METHODS ####################
void PartitionCamera::add_shared_listener(const boost::shared_ptr<Listener>& listener)
{
	m_listeners.add_shared_listener(listener);
}

void PartitionCamera::centre()
{
	SliceLocation loc(m_volumeSize[0] / 2, m_volumeSize[1] / 2, m_volumeSize[2] / 2, m_sliceLocation.layer);
	loc[m_sliceOrientation] = m_sliceLocation[m_sliceOrientation];
	set_slice_location(loc);
}

void PartitionCamera::goto_next_layer()
{
	SliceLocation loc = m_sliceLocation;
	++loc.layer;
	set_slice_location(loc);
}

void PartitionCamera::goto_next_slice()
{
	SliceLocation loc = m_sliceLocation;
	++loc[m_sliceOrientation];
	set_slice_location(loc);
}

void PartitionCamera::goto_previous_layer()
{
	SliceLocation loc = m_sliceLocation;
	--loc.layer;
	set_slice_location(loc);
}

void PartitionCamera::goto_previous_slice()
{
	SliceLocation loc = m_sliceLocation;
	--loc[m_sliceOrientation];
	set_slice_location(loc);
}

bool PartitionCamera::has_next_layer() const
{
	return m_sliceLocation.layer < m_highestLayer;
}

bool PartitionCamera::has_next_slice() const
{
	return m_sliceLocation[m_sliceOrientation] < static_cast<long>(m_volumeSize[m_sliceOrientation]) - 1;
}

bool PartitionCamera::has_previous_layer() const
{
	return  m_sliceLocation.layer > 1;
}

bool PartitionCamera::has_previous_slice() const
{
	return m_sliceLocation[m_sliceOrientation] > 0;
}

int PartitionCamera::max_zoom_level() const
{
	return 10;
}

int PartitionCamera::min_zoom_level() const
{
	return 0;
}

void PartitionCamera::pan_down()
{
	SliceLocation loc = m_sliceLocation;
	switch(m_sliceOrientation)
	{
		case ORIENT_XY:
			loc.y += PANNING_SPEED;
			break;
		case ORIENT_XZ:
		case ORIENT_YZ:
			loc.z += PANNING_SPEED;
			break;
	}
	set_slice_location(loc);
}

void PartitionCamera::pan_left()
{
	SliceLocation loc = m_sliceLocation;
	switch(m_sliceOrientation)
	{
		case ORIENT_XY:
		case ORIENT_XZ:
			loc.x -= PANNING_SPEED;
			break;
		case ORIENT_YZ:
			loc.y -= PANNING_SPEED;
			break;
	}
	set_slice_location(loc);
}

void PartitionCamera::pan_right()
{
	SliceLocation loc = m_sliceLocation;
	switch(m_sliceOrientation)
	{
		case ORIENT_XY:
		case ORIENT_XZ:
			loc.x += PANNING_SPEED;
			break;
		case ORIENT_YZ:
			loc.y += PANNING_SPEED;
			break;
	}
	set_slice_location(loc);
}

void PartitionCamera::pan_up()
{
	SliceLocation loc = m_sliceLocation;
	switch(m_sliceOrientation)
	{
		case ORIENT_XY:
			loc.y -= PANNING_SPEED;
			break;
		case ORIENT_XZ:
		case ORIENT_YZ:
			loc.z -= PANNING_SPEED;
			break;
	}
	set_slice_location(loc);
}

void PartitionCamera::set_highest_layer(int highestLayer)
{
	m_highestLayer = highestLayer;
}

void PartitionCamera::set_slice_location(const SliceLocation& sliceLocation)
{
	if(check_slice_location(sliceLocation))
	{
		bool sliceChanged = sliceLocation[m_sliceOrientation] != m_sliceLocation[m_sliceOrientation];
		bool layerChanged = sliceLocation.layer != m_sliceLocation.layer;
		m_sliceLocation = sliceLocation;
		m_listeners.slice_location_changed(sliceChanged, layerChanged);
	}
}

void PartitionCamera::set_slice_orientation(SliceOrientation sliceOrientation)
{
	m_sliceOrientation = sliceOrientation;
	m_listeners.slice_orientation_changed();
}

bool PartitionCamera::set_zoom_level(int zoomLevel)
{
	if(zoomLevel < min_zoom_level() || zoomLevel > max_zoom_level()) return false;
	m_zoomLevel = zoomLevel;
	m_listeners.zoom_level_changed();
	return true;
}

const SliceLocation& PartitionCamera::slice_location() const
{
	return m_sliceLocation;
}

SliceOrientation PartitionCamera::slice_orientation() const
{
	return m_sliceOrientation;
}

double PartitionCamera::zoom_factor() const
{
	return zoom_factor(zoom_level());
}

double PartitionCamera::zoom_factor(int zoomLevel) const
{
	return pow(1.25, zoomLevel);
}

int PartitionCamera::zoom_level() const
{
	return m_zoomLevel;
}

//#################### PRIVATE METHODS ####################
bool PartitionCamera::check_slice_location(const SliceLocation& loc) const
{
	itk::Index<3> size = ITKImageUtil::make_index_from_size(m_volumeSize);
	if(loc.x < 0 || loc.y < 0 || loc.z < 0 || loc.layer < 0 || loc.x >= size[0] || loc.y >= size[1] || loc.z >= size[2] || loc.layer > m_highestLayer)
	{
		return false;
	}
	return true;
}

}
