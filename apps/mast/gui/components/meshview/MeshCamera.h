/***
 * millipede: MeshCamera.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHCAMERA
#define H_MILLIPEDE_MESHCAMERA

#include <boost/bind.hpp>

#include <millipede/listeners/CompositeListenerBase.h>

namespace mp {

class MeshCamera
{
	//#################### LISTENERS ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void camera_changed() = 0;
	};

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void camera_changed()	{ multicast(boost::bind(&Listener::camera_changed, _1)); }
	};

	//#################### PRIVATE VARIABLES ####################
private:
	CompositeListener m_listeners;	///< the composite listener that stores all the camera's listeners

	//#################### DESTRUCTOR ####################
public:
	virtual ~MeshCamera() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void use_as_view() const = 0;

	//#################### PUBLIC METHODS ####################
public:
	void add_shared_listener(const boost::shared_ptr<Listener>& listener)
	{
		m_listeners.add_shared_listener(listener);
	}

	//#################### PROTECTED METHODS ####################
protected:
	void camera_changed()
	{
		m_listeners.camera_changed();
	}
};

}

#endif
