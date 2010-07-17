/***
 * millipede: ShaderProgram.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "ShaderProgram.h"

#include <common/exceptions/Exception.h>

namespace mp {

//#################### HELPER CLASSES ####################
struct ShaderProgramDeleter
{
	void operator()(void *p)
	{
		GLuint *id = static_cast<GLuint*>(p);
		if(glIsProgram(*id)) glDeleteProgram(*id);
		delete id;
	}
};

//#################### CONSTRUCTORS ####################
ShaderProgram::ShaderProgram()
:	m_id(new GLuint(glCreateProgram()), ShaderProgramDeleter())
{}

//#################### PUBLIC METHODS ####################
void ShaderProgram::attach_shader(const Shader& shader)
{
	glAttachShader(*m_id, shader.id());
	m_shaders.push_back(shader);			// we store the shader in order to ensure that its reference count doesn't drop to zero
}

GLuint ShaderProgram::id() const
{
	return *m_id;
}

void ShaderProgram::link()
{
	glLinkProgram(*m_id);
	int status;
	glGetProgramiv(*m_id, GL_LINK_STATUS, &status);
	if(status != GL_TRUE) throw Exception("Shader program could not be linked");
}

void ShaderProgram::use()
{
	glUseProgram(*m_id);
}

void ShaderProgram::use_fixed_functionality()
{
	glUseProgram(0);
}

}
