/***
 * millipede: PartitionOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONOVERLAY
#define H_MILLIPEDE_PARTITIONOVERLAY

#include <string>

#include <boost/shared_ptr.hpp>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class Texture> Texture_Ptr;

class PartitionOverlay
{
	//#################### PRIVATE VARIABLES ####################
private:
	Texture_Ptr m_texture;

	//#################### DESTRUCTOR ####################
public:
	virtual ~PartitionOverlay();

	//#################### PUBLIC METHODS ####################
public:
	virtual bool on_dicom_canvas() const;
	virtual bool on_partition_canvas() const;
	void render(double left, double top, double right, double bottom) const;

	//#################### PROTECTED METHODS ####################
protected:
	void set_texture(const Texture_Ptr& texture);
};

}

#endif
