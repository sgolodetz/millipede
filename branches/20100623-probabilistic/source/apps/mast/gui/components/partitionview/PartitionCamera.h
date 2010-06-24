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

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class ICommandManager> ICommandManager_Ptr;
typedef boost::shared_ptr<class SliceTextureSet> SliceTextureSet_Ptr;
typedef boost::shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;

class PartitionCamera
{
	//#################### NESTED CLASSES (EXCLUDING COMMANDS) ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void slice_location_changed(bool sliceChanged, bool layerChanged) = 0;
		virtual void slice_orientation_changed() = 0;
		virtual void texture_set_changed() = 0;
		virtual void zoom_level_changed() = 0;
	};

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void slice_location_changed(bool sliceChanged, bool layerChanged)	{ multicast(boost::bind(&Listener::slice_location_changed, _1, sliceChanged, layerChanged)); }
		void slice_orientation_changed()									{ multicast(boost::bind(&Listener::slice_orientation_changed, _1)); }
		void texture_set_changed()											{ multicast(boost::bind(&Listener::texture_set_changed, _1)); }
		void zoom_level_changed()											{ multicast(boost::bind(&Listener::zoom_level_changed, _1)); }
	};

	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_commandManager;
	SliceTextureSet_Ptr m_dicomTextureSet;
	CompositeListener m_listeners;
	std::vector<SliceTextureSet_Ptr> m_partitionTextureSets;
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
	SliceTextureSet_CPtr dicom_texture_set() const;
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
	SliceTextureSet_CPtr partition_texture_set(int layer) const;
	void set_command_manager(const ICommandManager_Ptr& commandManager);
	void set_dicom_texture_set(const SliceTextureSet_Ptr& dicomTextureSet);
	void set_slice_location(const SliceLocation& sliceLocation);
	void set_slice_orientation(SliceOrientation sliceOrientation);
	void set_partition_texture_sets(const std::vector<SliceTextureSet_Ptr>& partitionTextureSets);
	bool set_zoom_level(int zoomLevel);
	const SliceLocation& slice_location() const;
	SliceOrientation slice_orientation() const;
	double zoom_factor() const;
	double zoom_factor(int zoomLevel) const;
	int zoom_level() const;

	//#################### PRIVATE METHODS ####################
private:
	bool check_slice_location(const SliceLocation& loc) const;
	int highest_layer() const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<class PartitionCamera> PartitionCamera_Ptr;
typedef boost::shared_ptr<const class PartitionCamera> PartitionCamera_CPtr;

}

#endif
