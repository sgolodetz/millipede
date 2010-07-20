/***
 * millipede: Texture.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TEXTURE
#define H_MILLIPEDE_TEXTURE

#include <boost/shared_ptr.hpp>

#include <common/ogl/WrappedGL.h>

namespace mp {

/**
This class represents OpenGL textures. Essentially it's just a simple wrapper for an OpenGL texture ID,
but with reloading capabilities (i.e. the texture will reload itself if the screen resolution is changed).
*/
class Texture
{
	//#################### PROTECTED VARIABLES ####################
protected:
	bool m_clamp;
	mutable boost::shared_ptr<GLuint> m_id;

	//#################### CONSTRUCTORS ####################
protected:
	explicit Texture(bool clamp);

	//#################### DESTRUCTOR ####################
public:
	virtual ~Texture();

	//#################### PRIVATE ABSTRACT METHODS ####################
private:
	virtual void reload_image() const = 0;

	//#################### PUBLIC METHODS ####################
public:
	virtual void bind() const;
	virtual void reload() const;
	virtual void reload_partial(int minX, int minY, int maxX, int maxY) const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<Texture> Texture_Ptr;
typedef boost::shared_ptr<const Texture> Texture_CPtr;

}

#endif
