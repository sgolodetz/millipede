/***
 * millipede: Shader.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SHADER
#define H_MILLIPEDE_SHADER

#include <iosfwd>
#include <string>

#include <boost/shared_ptr.hpp>

#include <common/ogl/WrappedGL.h>

namespace mp {

class Shader
{
	//#################### PRIVATE VARIABLES ####################
private:
	boost::shared_ptr<GLuint> m_id;

	//#################### CONSTRUCTORS ####################
public:
	Shader(const std::string& source, GLenum shaderType);

	//#################### PUBLIC METHODS ####################
public:
	void compile();
	GLuint id() const;
	static Shader load_and_compile_fragment_shader(const std::string& filename);
	static Shader load_and_compile_vertex_shader(const std::string& filename);
	static Shader load_fragment_shader(const std::string& filename);
	static Shader load_vertex_shader(const std::string& filename);

	//#################### PRIVATE METHODS ####################
private:
	static std::string read_source(const std::string& filename);
};

}

#endif
