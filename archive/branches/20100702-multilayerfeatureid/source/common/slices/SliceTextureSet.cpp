/***
 * millipede: SliceTextureSet.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "SliceTextureSet.h"

namespace mp {

//#################### PUBLIC METHODS ####################
bool SliceTextureSet::has_textures(SliceOrientation ori) const
{
	return !m_textures[ori].empty();
}

void SliceTextureSet::set_textures(SliceOrientation ori, const std::vector<Texture_Ptr>& textures)
{
	m_textures[ori] = textures;
}

Texture_CPtr SliceTextureSet::texture(SliceOrientation ori, int n) const
{
	if(0 <= n && n < static_cast<int>(m_textures[ori].size())) return m_textures[ori][n];
	else return Texture_CPtr();
}

}
