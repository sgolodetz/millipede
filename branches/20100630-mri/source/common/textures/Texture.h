/***
 * millipede: Texture.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TEXTURE
#define H_MILLIPEDE_TEXTURE

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

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
	mutable shared_ptr<GLuint> m_id;

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
	void bind() const;

	//#################### PROTECTED METHODS ####################
protected:
	void reload() const;
	void set_id(GLuint id) const;
};

}

#endif
