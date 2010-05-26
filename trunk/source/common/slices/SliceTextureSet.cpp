/***
 * millipede: SliceTextureSet.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "SliceTextureSet.h"

namespace mp {

//#################### PUBLIC METHODS ####################
void SliceTextureSet::set_textures(SliceOrientation ori, const std::vector<Texture_Ptr>& textures)
{
	m_textures[ori] = textures;
}

Texture_CPtr SliceTextureSet::texture(SliceOrientation ori, int n) const
{
	assert(0 <= n && n < static_cast<int>(m_textures[ori].size()));
	return m_textures[ori][n];
}

}
