/***
 * millipede: Texture.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Texture.h"

#ifndef GL_CLAMP_TO_EDGE
	#define GL_CLAMP_TO_EDGE 0x812F		// this wrapping mode is only defined in OpenGL 1.2, so it's not necessarily in the header (unfortunately)
#endif

#ifndef GL_GENERATE_MIPMAP
	#define GL_GENERATE_MIPMAP 0x8191	// this isn't in the normal header either
#endif

namespace mp {

//#################### HELPER CLASSES ####################
struct TextureDeleter
{
	void operator()(void *p)
	{
		GLuint *id = static_cast<GLuint*>(p);
		if(glIsTexture(*id)) glDeleteTextures(1, id);
		delete id;
	}
};

//#################### CONSTRUCTORS ####################
/**
Constructs an empty texture (the subclass constructor will initialise it).

@param clamp	Whether or not the texture should be clamped to its edges (rather than wrapped)
*/
Texture::Texture(bool clamp)
:	m_clamp(clamp)
{}

//#################### DESTRUCTOR ####################
Texture::~Texture()
{}

//#################### PUBLIC METHODS ####################
/**
Binds the texture to GL_TEXTURE_2D (reloads it first if necessary).
*/
void Texture::bind() const
{
	if(!glIsTexture(*m_id)) reload();
	glBindTexture(GL_TEXTURE_2D, *m_id);
}

/**
Reloads the texture.
*/
void Texture::reload() const
{
	// Step 1:	If there's a texture currently loaded, delete it.
	if(m_id && glIsTexture(*m_id))
	{
		glDeleteTextures(1, m_id.get());
		m_id.reset();	// technically redundant, but clearer when debugging
	}

	// Step 2:	Generate a new texture ID.
	GLuint id;
	glGenTextures(1, &id);
	m_id.reset(new GLuint(id), TextureDeleter());

	// Step 3:	Bind the texture and set up the texture parameters.
	glBindTexture(GL_TEXTURE_2D, id);

	glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);

	// Enable trilinear filtering for this texture when minifying.
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

	// Clamp the texture if necessary (useful for things like lightmaps, for example).
	if(m_clamp)
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	}

	// Step 4:	Reload the actual image.
	reload_image();
}

}
