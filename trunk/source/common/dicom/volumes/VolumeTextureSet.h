/***
 * millipede: VolumeTextureSet.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMETEXTURESET
#define H_MILLIPEDE_VOLUMETEXTURESET

#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <itkImage.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class Texture> Texture_Ptr;
typedef shared_ptr<const class Texture> Texture_CPtr;

class VolumeTextureSet
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<Texture_Ptr> m_xyTextures, m_xzTextures, m_yzTextures;

	//#################### CONSTRUCTORS ####################
public:
	// Note: The second parameter is a dummy used to enable the compiler to deduce TPixel.
	template <typename TPixel> VolumeTextureSet(const typename itk::Image<TPixel,3>::ConstPointer& volumeImage, const itk::Image<TPixel,3>&);

	//#################### PUBLIC METHODS ####################
public:
	Texture_CPtr xy_texture(int n) const;
	Texture_CPtr xz_texture(int n) const;
	Texture_CPtr yz_texture(int n) const;
};

}

#include "VolumeTextureSet.tpp"

#endif
