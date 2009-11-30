/***
 * millipede: VolumeTextureSet.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeTextureSet.h"

namespace mp {

//#################### PUBLIC METHODS ####################
Texture_CPtr VolumeTextureSet::xy_texture(int n) const
{
	assert(0 <= n && n < static_cast<int>(m_xyTextures.size()));
	return m_xyTextures[n];
}

Texture_CPtr VolumeTextureSet::xz_texture(int n) const
{
	assert(0 <= n && n < static_cast<int>(m_xzTextures.size()));
	return m_xzTextures[n];
}

Texture_CPtr VolumeTextureSet::yz_texture(int n) const
{
	assert(0 <= n && n < static_cast<int>(m_yzTextures.size()));
	return m_yzTextures[n];
}

}
