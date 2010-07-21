/***
 * millipede: SliceTextureSet.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SLICETEXTURESET
#define H_MILLIPEDE_SLICETEXTURESET

#include <vector>

#include <common/textures/ITKImageTexture.h>
#include "SliceOrientation.h"

namespace mp {

template <typename TPixel>
class SliceTextureSet
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<Texture_Ptr> m_textures[3];

	//#################### PUBLIC METHODS ####################
public:
	bool has_textures(SliceOrientation ori) const
	{
		return !m_textures[ori].empty();
	}

	void set_textures(SliceOrientation ori, const std::vector<Texture_Ptr>& textures)
	{
		m_textures[ori] = textures;
	}

	Texture_CPtr texture(SliceOrientation ori, int n) const
	{
		if(0 <= n && n < static_cast<int>(m_textures[ori].size())) return m_textures[ori][n];
		else return Texture_CPtr();
	}
};

//#################### TYPEDEFS ####################
typedef SliceTextureSet<unsigned char> Greyscale8SliceTextureSet;
typedef boost::shared_ptr<Greyscale8SliceTextureSet> Greyscale8SliceTextureSet_Ptr;
typedef boost::shared_ptr<const Greyscale8SliceTextureSet> Greyscale8SliceTextureSet_CPtr;

}

#endif
