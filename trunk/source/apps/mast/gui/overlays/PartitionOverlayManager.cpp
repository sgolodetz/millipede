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
	std::string m_name;

	explicit NamePred(const std::string& name)
	:	m_name(name)
	{}

	bool operator()(const std::pair<std::string,PartitionOverlay_Ptr>& p) const
	{
		return p.first == m_name;
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

void PartitionOverlayManager::insert_overlay_above(const std::string& name, PartitionOverlay *overlay, const std::string& otherName)
{
	OverlayIter it = find_overlay(otherName);
	if(it == m_overlays.end()) throw Exception("No such overlay: " + otherName);
	++it;
	m_overlays.insert(it, std::make_pair(name, PartitionOverlay_Ptr(overlay)));
}

void PartitionOverlayManager::insert_overlay_at_bottom(const std::string& name, PartitionOverlay *overlay)
{
	m_overlays.push_front(std::make_pair(name, PartitionOverlay_Ptr(overlay)));
}

void PartitionOverlayManager::insert_overlay_at_top(const std::string& name, PartitionOverlay *overlay)
{
	m_overlays.push_back(std::make_pair(name, PartitionOverlay_Ptr(overlay)));
}

void PartitionOverlayManager::insert_overlay_below(const std::string& name, PartitionOverlay *overlay, const std::string& otherName)
{
	OverlayIter it = find_overlay(otherName);
	if(it == m_overlays.end()) throw Exception("No such overlay: " + otherName);
	m_overlays.insert(it, std::make_pair(name, PartitionOverlay_Ptr(overlay)));
}

void PartitionOverlayManager::render_dicom_overlays(double left, double top, double right, double bottom) const
{
	for(OverlayCIter it=m_overlays.begin(), iend=m_overlays.end(); it!=iend; ++it)
	{
		if(it->second->on_dicom_canvas())
		{
			it->second->render(left, top, right, bottom);
		}
	}
}

void PartitionOverlayManager::render_partition_overlays(double left, double top, double right, double bottom) const
{
	for(OverlayCIter it=m_overlays.begin(), iend=m_overlays.end(); it!=iend; ++it)
	{
		if(it->second->on_partition_canvas())
		{
			it->second->render(left, top, right, bottom);
		}
	}
}

void PartitionOverlayManager::replace_overlay(const std::string& name, PartitionOverlay *overlay)
{
	OverlayIter it = find_overlay(name);
	if(it == m_overlays.end()) throw Exception("No such overlay: " + name);
	it->second = PartitionOverlay_Ptr(overlay);
}

//#################### PRIVATE METHODS ####################
PartitionOverlayManager::OverlayIter PartitionOverlayManager::find_overlay(const std::string& name)
{
	return std::find_if(m_overlays.begin(), m_overlays.end(), NamePred(name));
}

}
