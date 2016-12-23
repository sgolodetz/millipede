/***
 * millipede: ShaderProgram.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SHADERPROGRAM
#define H_MILLIPEDE_SHADERPROGRAM

#include <vector>

#include <common/ogl/WrappedGL.h>
#include "Shader.h"

namespace mp {

class ShaderProgram
{
	//#################### PRIVATE VARIABLES ####################
private:
	boost::shared_ptr<GLuint> m_id;
	std::vector<Shader> m_shaders;

	//#################### CONSTRUCTORS ####################
public:
	ShaderProgram();

	//#################### PUBLIC METHODS ####################
public:
	void attach_shader(const Shader& shader);
	GLuint id() const;
	void link();
	void use();
	static void use_fixed_functionality();
};

}

#endif
