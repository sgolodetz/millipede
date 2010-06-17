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
typedef boost::shared_ptr<class SliceTextureSet> SliceTextureSet_Ptr;
typedef boost::shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;

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

	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_commandManager;
	SliceTextureSet_Ptr m_dicomTextureSet;
	std::vector<Listener*> m_listeners;
	std::vector<SliceTextureSet_Ptr> m_partitionTextureSets;
	SliceLocation m_sliceLocation;	// slice location in terms of the volume only (not based on actual slice numbers)
	SliceOrientation m_sliceOrientation;
	itk::Size<3> m_volumeSize;

	//#################### CONSTRUCTORS ####################
public:
	PartitionCamera(const SliceLocation& sliceLocation, SliceOrientation sliceOrientation, const itk::Size<3>& volumeSize);

	//#################### PUBLIC METHODS ####################
public:
	void add_listener(Listener *listener);
	void change_slice_location(const SliceLocation& oldSliceLocation, const SliceLocation& sliceLocation, const std::string& commandDescription);
	SliceTextureSet_CPtr dicom_texture_set() const;
	SliceTextureSet_CPtr partition_texture_set(int layer) const;
	void set_command_manager(const ICommandManager_Ptr& commandManager);
	void set_dicom_texture_set(const SliceTextureSet_Ptr& dicomTextureSet);
	void set_slice_location(const SliceLocation& sliceLocation);
	void set_slice_orientation(SliceOrientation sliceOrientation);
	void set_partition_texture_sets(const std::vector<SliceTextureSet_Ptr>& partitionTextureSets);
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
