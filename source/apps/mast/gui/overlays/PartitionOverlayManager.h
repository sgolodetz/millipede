/***
 * millipede: PartitionOverlayManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONOVERLAYMANAGER
#define H_MILLIPEDE_PARTITIONOVERLAYMANAGER

#include <list>
#include <string>
#include <utility>

#include <boost/shared_ptr.hpp>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class PartitionOverlay> PartitionOverlay_Ptr;

class PartitionOverlayManager
{
	//#################### TYPEDEFS ####################
private:
	typedef std::list<std::pair<std::string,PartitionOverlay_Ptr> > OverlayList;
	typedef OverlayList::iterator OverlayIter;
	typedef OverlayList::const_iterator OverlayCIter;

	//#################### PRIVATE VARIABLES ####################
private:
	OverlayList m_overlays;

	//#################### PUBLIC METHODS ####################
public:
	void clear_overlays();
	void erase_overlay(const std::string& name);
	void insert_overlay_above(const std::string& name, PartitionOverlay *overlay, const std::string& otherName);
	void insert_overlay_at_bottom(const std::string& name, PartitionOverlay *overlay);
	void insert_overlay_at_top(const std::string& name, PartitionOverlay *overlay);
	void insert_overlay_below(const std::string& name, PartitionOverlay *overlay, const std::string& otherName);
	void render_dicom_overlays(double left, double top, double right, double bottom) const;
	void render_partition_overlays(double left, double top, double right, double bottom) const;
	void replace_overlay(const std::string& name, PartitionOverlay *overlay);

	//#################### PRIVATE METHODS ####################
private:
	OverlayIter find_overlay(const std::string& name);
};

}

#endif
