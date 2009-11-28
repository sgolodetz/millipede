/***
 * millipede: ViewedVolume.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VIEWEDVOLUME
#define H_MILLIPEDE_VIEWEDVOLUME

#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class ViewedVolumeListener> ViewedVolumeListener_Ptr;
typedef shared_ptr<class Volume> Volume_Ptr;
typedef shared_ptr<const class Volume> Volume_CPtr;
typedef shared_ptr<class VolumeTextureSet> VolumeTextureSet_Ptr;
typedef shared_ptr<const class VolumeTextureSet> VolumeTextureSet_CPtr;

class ViewedVolume
{
	//#################### ENUMERATIONS ####################
public:
	enum ViewOrientation
	{
		ORIENT_XY,
		ORIENT_XZ,
		ORIENT_YZ,
	};

	//#################### NESTED CLASSES ####################
public:
	struct ViewLocation
	{
		int x, y, z, layer;

		ViewLocation(int x_, int y_, int z_, int layer_) : x(x_), y(y_), z(z_), layer(layer_) {}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	ViewLocation m_viewLocation;		// view location in terms of the volume only (not based on actual slice numbers)
	ViewOrientation m_viewOrientation;
	Volume_Ptr m_volume;
	VolumeTextureSet_Ptr m_textureSet;

	std::vector<ViewedVolumeListener*> m_listeners;

	//#################### CONSTRUCTORS ####################
public:
	ViewedVolume(const Volume_Ptr& volume, const ViewLocation& loc, ViewOrientation ori);

	//#################### PUBLIC METHODS ####################
public:
	void add_listener(ViewedVolumeListener *listener);
	void set_view_location(const ViewLocation& loc);
	void set_view_orientation(ViewOrientation ori);
	void set_volume_texture_set(const VolumeTextureSet_Ptr& textureSet);
	const ViewLocation& view_location() const;
	ViewOrientation view_orientation() const;
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
