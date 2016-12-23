/***
 * millipede: PartitionOverlayManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONOVERLAYMANAGER
#define H_MILLIPEDE_PARTITIONOVERLAYMANAGER

#include <list>

#include <boost/shared_ptr.hpp>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class PartitionOverlay> PartitionOverlay_Ptr;

class PartitionOverlayManager
{
	//#################### TYPEDEFS ####################
private:
	typedef std::list<PartitionOverlay_Ptr>::iterator OverlayIter;
	typedef std::list<PartitionOverlay_Ptr>::const_iterator OverlayCIter;

	//#################### PRIVATE VARIABLES ####################
private:
	std::list<PartitionOverlay_Ptr> m_overlays;

	//#################### PUBLIC METHODS ####################
public:
	void clear_overlays();
	void erase_overlay(const std::string& name);
	void insert_overlay_above(PartitionOverlay *overlay, const std::string& otherName);
	void insert_overlay_at_bottom(PartitionOverlay *overlay);
	void insert_overlay_at_top(PartitionOverlay *overlay);
	void insert_overlay_below(PartitionOverlay *overlay, const std::string& otherName);
	void render_dicom_overlays(double left, double top, double right, double bottom) const;
	void render_partition_overlays(double left, double top, double right, double bottom) const;

	//#################### PRIVATE METHODS ####################
private:
	OverlayIter find_overlay(const std::string& name);
};

}

#endif
