/***
 * millipede: ViewedVolume.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VIEWEDVOLUME
#define H_MILLIPEDE_VIEWEDVOLUME

#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <common/dicom/volumes/SliceOrientation.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class ViewedVolumeListener> ViewedVolumeListener_Ptr;
typedef shared_ptr<class Volume> Volume_Ptr;
typedef shared_ptr<const class Volume> Volume_CPtr;
typedef shared_ptr<class VolumeTextureSet> VolumeTextureSet_Ptr;
typedef shared_ptr<const class VolumeTextureSet> VolumeTextureSet_CPtr;

class ViewedVolume
{
	//#################### NESTED CLASSES ####################
public:
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

	std::vector<ViewedVolumeListener*> m_listeners;

	//#################### CONSTRUCTORS ####################
public:
	ViewedVolume(const Volume_Ptr& volume, const ViewLocation& loc, SliceOrientation ori);

	//#################### PUBLIC METHODS ####################
public:
	void add_listener(ViewedVolumeListener *listener);
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
typedef shared_ptr<ViewedVolume> ViewedVolume_Ptr;
typedef shared_ptr<const ViewedVolume> ViewedVolume_CPtr;

}

#endif
