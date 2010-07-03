/***
 * millipede: PartitionCamera.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONCAMERA
#define H_MILLIPEDE_PARTITIONCAMERA

#include <vector>

#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>

#include <itkSize.h>

#include <common/listeners/CompositeListenerBase.h>
#include <common/slices/SliceLocation.h>
#include <common/slices/SliceOrientation.h>

namespace mp {

class PartitionCamera
{
	//#################### LISTENERS ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void slice_location_changed(bool sliceChanged, bool layerChanged) = 0;
		virtual void slice_orientation_changed() = 0;
		virtual void zoom_level_changed() = 0;
	};

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void slice_location_changed(bool sliceChanged, bool layerChanged)	{ multicast(boost::bind(&Listener::slice_location_changed, _1, sliceChanged, layerChanged)); }
		void slice_orientation_changed()									{ multicast(boost::bind(&Listener::slice_orientation_changed, _1)); }
		void zoom_level_changed()											{ multicast(boost::bind(&Listener::zoom_level_changed, _1)); }
	};

	//#################### PRIVATE VARIABLES ####################
private:
	int m_highestLayer;
	CompositeListener m_listeners;
	SliceLocation m_sliceLocation;	// slice location in terms of the volume only (not based on actual slice numbers)
	SliceOrientation m_sliceOrientation;
	itk::Size<3> m_volumeSize;
	int m_zoomLevel;

	//#################### CONSTRUCTORS ####################
public:
	PartitionCamera(const SliceLocation& sliceLocation, SliceOrientation sliceOrientation, const itk::Size<3>& volumeSize);

	//#################### PUBLIC METHODS ####################
public:
	void add_shared_listener(const boost::shared_ptr<Listener>& listener);
	void centre();
	void goto_next_layer();
	void goto_next_slice();
	void goto_previous_layer();
	void goto_previous_slice();
	bool has_next_layer() const;
	bool has_next_slice() const;
	bool has_previous_layer() const;
	bool has_previous_slice() const;
	int max_zoom_level() const;
	int min_zoom_level() const;
	void pan_down();
	void pan_left();
	void pan_right();
	void pan_up();
	void set_highest_layer(int highestLayer);
	void set_slice_location(const SliceLocation& sliceLocation);
	void set_slice_orientation(SliceOrientation sliceOrientation);
	bool set_zoom_level(int zoomLevel);
	const SliceLocation& slice_location() const;
	SliceOrientation slice_orientation() const;
	double zoom_factor() const;
	double zoom_factor(int zoomLevel) const;
	int zoom_level() const;

	//#################### PRIVATE METHODS ####################
private:
	bool check_slice_location(const SliceLocation& loc) const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<class PartitionCamera> PartitionCamera_Ptr;
typedef boost::shared_ptr<const class PartitionCamera> PartitionCamera_CPtr;

}

#endif
