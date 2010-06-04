/***
 * millipede: PartitionOverlayManager.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "PartitionOverlayManager.h"

#include <common/exceptions/Exception.h>
#include "PartitionOverlay.h"
using namespace mp;

namespace {

//#################### LOCAL CLASSES ####################
struct NamePred
{
	std::string m_otherName;

	explicit NamePred(const std::string& otherName)
	:	m_otherName(otherName)
	{}

	bool operator()(const PartitionOverlay_Ptr& overlay) const
	{
		return overlay->name() == m_otherName;
	}
};

}

namespace mp {

//#################### PUBLIC METHODS ####################
void PartitionOverlayManager::clear_overlays()
{
	m_overlays.clear();
}

void PartitionOverlayManager::erase_overlay(const std::string& name)
{
	m_overlays.erase(find_overlay(name));
}

void PartitionOverlayManager::insert_overlay_above(PartitionOverlay *overlay, const std::string& otherName)
{
	OverlayIter it = find_overlay(otherName);
	if(it == m_overlays.end()) throw Exception("No such overlay: " + otherName);
	++it;
	m_overlays.insert(it, PartitionOverlay_Ptr(overlay));
}

void PartitionOverlayManager::insert_overlay_at_bottom(PartitionOverlay *overlay)
{
	m_overlays.push_front(PartitionOverlay_Ptr(overlay));
}

void PartitionOverlayManager::insert_overlay_at_top(PartitionOverlay *overlay)
{
	m_overlays.push_back(PartitionOverlay_Ptr(overlay));
}

void PartitionOverlayManager::insert_overlay_below(PartitionOverlay *overlay, const std::string& otherName)
{
	OverlayIter it = find_overlay(otherName);
	if(it == m_overlays.end()) throw Exception("No such overlay: " + otherName);
	m_overlays.insert(it, PartitionOverlay_Ptr(overlay));
}

void PartitionOverlayManager::render_dicom_overlays(double left, double top, double right, double bottom) const
{
	for(OverlayCIter it=m_overlays.begin(), iend=m_overlays.end(); it!=iend; ++it)
	{
		if((*it)->on_dicom_canvas())
		{
			(*it)->render(left, top, right, bottom);
		}
	}
}

void PartitionOverlayManager::render_partition_overlays(double left, double top, double right, double bottom) const
{
	for(OverlayCIter it=m_overlays.begin(), iend=m_overlays.end(); it!=iend; ++it)
	{
		if((*it)->on_partition_canvas())
		{
			(*it)->render(left, top, right, bottom);
		}
	}
}

//#################### PRIVATE METHODS ####################
PartitionOverlayManager::OverlayIter PartitionOverlayManager::find_overlay(const std::string& name)
{
	return std::find_if(m_overlays.begin(), m_overlays.end(), NamePred(name));
}

}
