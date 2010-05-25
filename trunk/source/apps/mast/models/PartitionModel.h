/***
 * millipede: PartitionModel.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONMODEL
#define H_MILLIPEDE_PARTITIONMODEL

#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <common/dicom/volumes/SliceOrientation.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class Volume> Volume_Ptr;
typedef shared_ptr<const class Volume> Volume_CPtr;
typedef shared_ptr<class VolumeTextureSet> VolumeTextureSet_Ptr;
typedef shared_ptr<const class VolumeTextureSet> VolumeTextureSet_CPtr;

class PartitionModel
{
	//#################### NESTED CLASSES ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void model_changed() = 0;
	};

	struct ViewLocation
	{
		int x, y, z, layer;

		ViewLocation(int x_, int y_, int z_, int layer_)
		:	x(x_), y(y_), z(z_), layer(layer_)
		{}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	SliceOrientation m_sliceOrientation;
	ViewLocation m_viewLocation;			// view location in terms of the volume only (not based on actual slice numbers)
	Volume_Ptr m_volume;
	VolumeTextureSet_Ptr m_textureSet;

	std::vector<Listener*> m_listeners;

	//#################### CONSTRUCTORS ####################
public:
	PartitionModel(const Volume_Ptr& volume, const ViewLocation& loc, SliceOrientation ori);

	//#################### PUBLIC METHODS ####################
public:
	void add_listener(Listener *listener);
	void set_slice_orientation(SliceOrientation ori);
	void set_view_location(const ViewLocation& loc);
	void set_volume_texture_set(const VolumeTextureSet_Ptr& textureSet);
	SliceOrientation slice_orientation() const;
	const ViewLocation& view_location() const;
	const Volume_Ptr& volume();
	Volume_CPtr volume() const;
	const VolumeTextureSet_Ptr& volume_texture_set();
	VolumeTextureSet_CPtr volume_texture_set() const;

	//#################### PRIVATE METHODS ####################
private:
	void alert_listeners();
};

//#################### TYPEDEFS ####################
typedef shared_ptr<PartitionModel> PartitionModel_Ptr;
typedef shared_ptr<const PartitionModel> PartitionModel_CPtr;

}

#endif
