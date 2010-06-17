/***
 * millipede: PartitionCamera.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONCAMERA
#define H_MILLIPEDE_PARTITIONCAMERA

#include <vector>

#include <boost/shared_ptr.hpp>

#include <itkSize.h>

#include <common/slices/SliceLocation.h>
#include <common/slices/SliceOrientation.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class ICommandManager> ICommandManager_Ptr;

class PartitionCamera
{
	//#################### NESTED CLASSES (EXCLUDING COMMANDS) ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void camera_changed() = 0;
	};

	//#################### COMMANDS ####################
private:
	struct ChangeSliceLocationCommand;
	struct ChangeSliceOrientationCommand;

	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_commandManager;
	std::vector<Listener*> m_listeners;
	SliceLocation m_sliceLocation;			// slice location in terms of the volume only (not based on actual slice numbers)
	SliceOrientation m_sliceOrientation;
	itk::Size<3> m_volumeSize;

	//#################### CONSTRUCTORS ####################
public:
	PartitionCamera(const SliceLocation& sliceLocation, SliceOrientation sliceOrientation, const itk::Size<3>& volumeSize);

	//#################### PUBLIC METHODS ####################
public:
	void add_listener(Listener *listener);
	void change_slice_location(const SliceLocation& sliceLocation, const std::string& description);
	void change_slice_orientation(SliceOrientation sliceOrientation, const std::string& description);
	void set_command_manager(const ICommandManager_Ptr& commandManager);
	const SliceLocation& slice_location() const;
	SliceOrientation slice_orientation() const;

	//#################### PRIVATE METHODS ####################
private:
	void alert_listeners();
	void check_slice_location(const SliceLocation& sliceLocation) const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<class PartitionCamera> PartitionCamera_Ptr;
typedef boost::shared_ptr<const class PartitionCamera> PartitionCamera_CPtr;

}

#endif
