/***
 * millipede: ShaderProgram.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SHADERPROGRAM
#define H_MILLIPEDE_SHADERPROGRAM

#include <common/ogl/WrappedGL.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
class Shader;

class ShaderProgram
{
	//#################### PRIVATE VARIABLES ####################
private:
	GLuint m_id;

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
