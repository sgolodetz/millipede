/***
 * millipede: SliceTextureSet.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SLICETEXTURESET
#define H_MILLIPEDE_SLICETEXTURESET

#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <itkImage.h>

#include "SliceOrientation.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class Texture> Texture_Ptr;
typedef shared_ptr<const class Texture> Texture_CPtr;

class SliceTextureSet
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<Texture_Ptr> m_textures[3];

	//#################### PUBLIC METHODS ####################
public:
	void set_textures(SliceOrientation ori, const std::vector<Texture_Ptr>& textures);
	Texture_CPtr texture(SliceOrientation ori, int n) const;
};

}

#endif
