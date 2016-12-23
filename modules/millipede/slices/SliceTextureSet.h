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
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<TPixel,2> Image;
	typedef ITKImageTexture<Image> ITKImageTextureT;
	typedef boost::shared_ptr<ITKImageTextureT> ITKImageTexture_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<ITKImageTexture_Ptr> m_textures[3];

	//#################### CONSTRUCTORS ####################
public:
	SliceTextureSet()
	{}

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
public:
	SliceTextureSet(const SliceTextureSet& rhs)
	{
		// Perform a deep copy of all the textures.
		for(int i=0; i<3; ++i)
		{
			size_t size = rhs.m_textures[i].size();
			m_textures[i].resize(size);

			for(size_t j=0; j<size; ++j)
			{
				m_textures[i][j] = rhs.m_textures[i][j]->clone();
			}
		}
	}

private:
	SliceTextureSet& operator=(const SliceTextureSet&);

	//#################### PUBLIC METHODS ####################
public:
	const TPixel& get_pixel(SliceOrientation ori, const itk::Index<3>& index) const
	{
		switch(ori)
		{
			case ORIENT_XY:		return m_textures[ORIENT_XY][index[2]]->get_pixel(ITKImageUtil::make_index(index[0], index[1]));
			case ORIENT_XZ:		return m_textures[ORIENT_XZ][index[1]]->get_pixel(ITKImageUtil::make_index(index[0], index[2]));
			case ORIENT_YZ:		return m_textures[ORIENT_YZ][index[0]]->get_pixel(ITKImageUtil::make_index(index[1], index[2]));
			default:			throw Exception("Bad slice orientation");	// this should never happen
		}
	}

	bool has_textures(SliceOrientation ori) const
	{
		return !m_textures[ori].empty();
	}

	void set_pixel(SliceOrientation ori, const itk::Index<3>& index, const TPixel& pixel)
	{
		switch(ori)
		{
			case ORIENT_XY:		m_textures[ORIENT_XY][index[2]]->set_pixel(ITKImageUtil::make_index(index[0], index[1]), pixel); break;
			case ORIENT_XZ:		m_textures[ORIENT_XZ][index[1]]->set_pixel(ITKImageUtil::make_index(index[0], index[2]), pixel); break;
			case ORIENT_YZ:		m_textures[ORIENT_YZ][index[0]]->set_pixel(ITKImageUtil::make_index(index[1], index[2]), pixel); break;
			default:			throw Exception("Bad slice orientation");	// this should never happen
		}
	}

	void set_textures(SliceOrientation ori, const std::vector<ITKImageTexture_Ptr>& textures)
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
