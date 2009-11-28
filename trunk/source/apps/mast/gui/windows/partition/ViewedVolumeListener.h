/***
 * millipede: ViewedVolumeListener.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VIEWEDVOLUMELISTENER
#define H_MILLIPEDE_VIEWEDVOLUMELISTENER

namespace mp {

class ViewedVolumeListener
{
	//#################### DESTRUCTOR ####################
public:
	virtual ~ViewedVolumeListener() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void viewed_volume_changed() = 0;
};

}

#endif
