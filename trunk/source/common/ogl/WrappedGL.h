/***
 * millipede: WrappedGL.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifdef _WIN32
	#ifndef NOMINMAX
		#define NOMINMAX		// prevent the min and max macros in windows.h being defined (they interfere with the Standard C++ equivalents)
	#endif
	#include <windows.h>
#endif

#include <GL/gl.h>
