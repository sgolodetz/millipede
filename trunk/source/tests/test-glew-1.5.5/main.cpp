/***
 * test-glew-1.5.5: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>

#include <GL/glew.h>

int main()
{
	std::cout << "This is a test to make sure a program using GLEW can be built.\n";
	if(glewInit() != GLEW_OK)
	{
		std::cout << "As expected, GLEW could not actually be initialized (this is only a stub).\n";
	}
	return 0;
}
