/***
 * millipede: Shader.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "shaders/Shader.h"

#include <algorithm>
#include <fstream>
#include <sstream>

#include "exceptions/Exception.h"

namespace mp {

//#################### HELPER CLASSES ####################
struct ShaderDeleter
{
	void operator()(void *p)
	{
		GLuint *id = static_cast<GLuint*>(p);
		if(glIsShader(*id)) glDeleteShader(*id);
		delete id;
	}
};

//#################### CONSTRUCTORS ####################
Shader::Shader(const std::string& source, GLenum shaderType)
{
	const char *sourceStrings[] = {source.c_str()};
	m_id.reset(new GLuint(glCreateShader(shaderType)), ShaderDeleter());
	glShaderSource(*m_id, 1, sourceStrings, NULL);
}

//#################### PUBLIC METHODS ####################
void Shader::compile()
{
	glCompileShader(*m_id);
	int status;
	glGetShaderiv(*m_id, GL_COMPILE_STATUS, &status);
	if(status != GL_TRUE) throw Exception("Shader could not be compiled");
}

GLuint Shader::id() const
{
	return *m_id;
}

Shader Shader::load_and_compile_fragment_shader(const std::string& filename)
{
	Shader shader = load_fragment_shader(filename);
	shader.compile();
	return shader;
}

Shader Shader::load_and_compile_vertex_shader(const std::string& filename)
{
	Shader shader = load_vertex_shader(filename);
	shader.compile();
	return shader;
}

Shader Shader::load_fragment_shader(const std::string& filename)
{
	return Shader(read_source(filename), GL_FRAGMENT_SHADER);
}

Shader Shader::load_vertex_shader(const std::string& filename)
{
	return Shader(read_source(filename), GL_VERTEX_SHADER);
}

//#################### PRIVATE METHODS ####################
std::string Shader::read_source(const std::string& filename)
{
	std::ifstream fs(filename.c_str(), std::ios_base::binary);
	std::ostringstream os;
	std::copy(std::istreambuf_iterator<char>(fs), (std::istreambuf_iterator<char>()), std::ostreambuf_iterator<char>(os));
	return os.str();
}

}
