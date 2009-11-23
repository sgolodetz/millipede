/***
 * millipede: ViewedVolumeModel.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VIEWEDVOLUMEMODEL
#define H_MILLIPEDE_VIEWEDVOLUMEMODEL

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class Volume> Volume_Ptr;
typedef shared_ptr<class VolumeTextureSet> VolumeTextureSet_Ptr;

struct ViewedVolumeModel
{
	//#################### NESTED CLASSES ####################
	struct ViewLocation
	{
		int stratum;
		int layer;

		ViewLocation(int stratum_, int layer_) : stratum(stratum_), layer(layer_) {}
	};

	//#################### TYPEDEFS ####################
	typedef shared_ptr<ViewLocation> ViewLocation_Ptr;

	//#################### PUBLIC VARIABLES ####################
	ViewLocation_Ptr m_viewLocation;
	Volume_Ptr m_volume;
	VolumeTextureSet_Ptr m_textureSet;
};

}

#endif
