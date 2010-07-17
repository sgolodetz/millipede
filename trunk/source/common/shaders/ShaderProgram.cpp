/***
 * millipede: ShaderProgram.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "ShaderProgram.h"

#include <common/exceptions/Exception.h>
#include "Shader.h"

namespace mp {

//#################### CONSTRUCTORS ####################
ShaderProgram::ShaderProgram()
:	m_id(glCreateProgram())
{}

//#################### PUBLIC METHODS ####################
void ShaderProgram::attach_shader(const Shader& shader)
{
	glAttachShader(m_id, shader.id());
}

GLuint ShaderProgram::id() const
{
	return m_id;
}

void ShaderProgram::link()
{
	glLinkProgram(m_id);
	int status;
	glGetProgramiv(m_id, GL_LINK_STATUS, &status);
	if(status != GL_TRUE) throw Exception("Shader program could not be linked");
}

void ShaderProgram::use()
{
	glUseProgram(m_id);
}

void ShaderProgram::use_fixed_functionality()
{
	glUseProgram(0);
}

}
