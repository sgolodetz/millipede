/***
 * millipede: PartitionCamera.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "PartitionCamera.h"

#include <boost/optional.hpp>

#include <common/commands/BasicCommandManager.h>
#include <common/commands/Command.h>

namespace mp {

//#################### COMMANDS ####################
struct PartitionCamera::ChangeSliceLocationCommand : Command
{
	PartitionCamera *m_base;
	SliceLocation m_oldSliceLocation;
	SliceLocation m_sliceLocation;

	ChangeSliceLocationCommand(PartitionCamera *base, const SliceLocation& oldSliceLocation, const SliceLocation& sliceLocation, const std::string& description)
	:	Command(description), m_base(base), m_oldSliceLocation(oldSliceLocation), m_sliceLocation(sliceLocation)
	{}

	void execute()
	{
		m_base->m_sliceLocation = m_sliceLocation;
		m_base->alert_listeners();
	}

	void undo()
	{
		m_base->m_sliceLocation = m_oldSliceLocation;
		m_base->alert_listeners();
	}
};

//#################### CONSTRUCTORS ####################
PartitionCamera::PartitionCamera(const SliceLocation& sliceLocation, SliceOrientation sliceOrientation, const itk::Size<3>& volumeSize)
:	m_commandManager(new BasicCommandManager), m_sliceLocation(sliceLocation), m_sliceOrientation(sliceOrientation), m_volumeSize(volumeSize)
{
	check_slice_location(m_sliceLocation);
}

//#################### PUBLIC METHODS ####################
void PartitionCamera::add_listener(Listener *listener)
{
	m_listeners.push_back(listener);
}

SliceTextureSet_CPtr PartitionCamera::dicom_texture_set() const
{
	return m_dicomTextureSet;
}

void PartitionCamera::goto_next_layer()
{
	SliceLocation loc = m_sliceLocation;
	++loc.layer;
	goto_slice_location(m_sliceLocation, loc, "Goto Next Layer");
}

void PartitionCamera::goto_next_slice()
{
	SliceLocation loc = m_sliceLocation;
	++loc[m_sliceOrientation];
	goto_slice_location(m_sliceLocation, loc, "Goto Next Slice");
}

void PartitionCamera::goto_previous_layer()
{
	SliceLocation loc = m_sliceLocation;
	--loc.layer;
	goto_slice_location(m_sliceLocation, loc, "Goto Previous Layer");
}

void PartitionCamera::goto_previous_slice()
{
	SliceLocation loc = m_sliceLocation;
	--loc[m_sliceOrientation];
	goto_slice_location(m_sliceLocation, loc, "Goto Previous Slice");
}

void PartitionCamera::goto_slice_location(const SliceLocation& oldSliceLocation, const SliceLocation& sliceLocation, const std::string& commandDescription)
{
	check_slice_location(sliceLocation);
	m_commandManager->execute(Command_Ptr(new ChangeSliceLocationCommand(this, oldSliceLocation, sliceLocation, commandDescription)));
}

bool PartitionCamera::has_next_layer() const
{
	return m_sliceLocation.layer < highest_layer();
}

bool PartitionCamera::has_next_slice() const
{
	return m_sliceLocation[m_sliceOrientation] < m_volumeSize[m_sliceOrientation] - 1;
}

bool PartitionCamera::has_previous_layer() const
{
	return  m_sliceLocation.layer > 1;
}

bool PartitionCamera::has_previous_slice() const
{
	return m_sliceLocation[m_sliceOrientation] > 0;
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
	m_sliceLocation = sliceLocation;
	alert_listeners();
}

void PartitionCamera::set_slice_orientation(SliceOrientation sliceOrientation)
{
	m_sliceOrientation = sliceOrientation;
	alert_listeners();
}

const SliceLocation& PartitionCamera::slice_location() const
{
	return m_sliceLocation;
}

SliceOrientation PartitionCamera::slice_orientation() const
{
	return m_sliceOrientation;
}

//#################### PRIVATE METHODS ####################
void PartitionCamera::alert_listeners()
{
	for(size_t i=0, size=m_listeners.size(); i<size; ++i)
	{
		m_listeners[i]->camera_changed();
	}
}

void PartitionCamera::check_slice_location(const SliceLocation& sliceLocation) const
{
	// TODO
}

int PartitionCamera::highest_layer() const
{
	return static_cast<int>(m_partitionTextureSets.size());
}

}
