/***
 * millipede: MeshCamera.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHCAMERA
#define H_MILLIPEDE_MESHCAMERA

namespace mp {

class MeshCamera
{
	//#################### DESTRUCTOR ####################
public:
	virtual ~MeshCamera() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void use_as_view() const = 0;
};

}

#endif
