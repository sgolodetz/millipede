/***
 * millipede: PartitionCamera.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "PartitionCamera.h"

#include <boost/optional.hpp>

#include <common/commands/BasicCommandManager.h>
#include <common/commands/Command.h>
#include <common/exceptions/Exception.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionCamera::PartitionCamera(const SliceLocation& sliceLocation, SliceOrientation sliceOrientation, const itk::Size<3>& volumeSize)
:	m_commandManager(new BasicCommandManager), m_sliceLocation(sliceLocation), m_sliceOrientation(sliceOrientation), m_volumeSize(volumeSize), m_zoomLevel(0)
{
	check_slice_location(m_sliceLocation);
}

//#################### PUBLIC METHODS ####################
void PartitionCamera::add_listener(Listener *listener)
{
	m_listeners.push_back(listener);
}

void PartitionCamera::centre()
{
	SliceLocation loc(m_volumeSize[0] / 2, m_volumeSize[1] / 2, m_volumeSize[2] / 2, m_sliceLocation.layer);
	loc[m_sliceOrientation] = m_sliceLocation[m_sliceOrientation];
	set_slice_location(loc);
}

SliceTextureSet_CPtr PartitionCamera::dicom_texture_set() const
{
	return m_dicomTextureSet;
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
	return m_sliceLocation.layer < highest_layer();
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

SliceTextureSet_CPtr PartitionCamera::partition_texture_set(int layer) const
{
	int n = layer - 1;
	if(0 <= n && n < static_cast<int>(m_partitionTextureSets.size())) return m_partitionTextureSets[n];
	else return SliceTextureSet_CPtr();
}

void PartitionCamera::set_command_manager(const ICommandManager_Ptr& commandManager)
{
	m_commandManager = commandManager;
}

void PartitionCamera::set_dicom_texture_set(const SliceTextureSet_Ptr& dicomTextureSet)
{
	m_dicomTextureSet = dicomTextureSet;
	alert_listeners();
}

void PartitionCamera::set_partition_texture_sets(const std::vector<SliceTextureSet_Ptr>& partitionTextureSets)
{
	m_partitionTextureSets = partitionTextureSets;
	alert_listeners();
}

void PartitionCamera::set_slice_location(const SliceLocation& sliceLocation)
{
	check_slice_location(sliceLocation);
	m_sliceLocation = sliceLocation;
	alert_listeners();
}

void PartitionCamera::set_slice_orientation(SliceOrientation sliceOrientation)
{
	m_sliceOrientation = sliceOrientation;
	alert_listeners();
}

bool PartitionCamera::set_zoom_level(int zoomLevel)
{
	if(zoomLevel < min_zoom_level() || zoomLevel > max_zoom_level()) return false;
	m_zoomLevel = zoomLevel;
	alert_listeners();
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
void PartitionCamera::alert_listeners()
{
	for(size_t i=0, size=m_listeners.size(); i<size; ++i)
	{
		m_listeners[i]->camera_changed();
	}
}

void PartitionCamera::check_slice_location(const SliceLocation& loc) const
{
	itk::Index<3> size = ITKImageUtil::make_index_from_size(m_volumeSize);
	if(loc.x < 0 || loc.y < 0 || loc.z < 0 || loc.layer < 0 || loc.x >= size[0] || loc.y >= size[1] || loc.z >= size[2] || loc.layer > highest_layer())
	{
		throw Exception("Bad slice location");
	}
}

int PartitionCamera::highest_layer() const
{
	return static_cast<int>(m_partitionTextureSets.size());
}

}
