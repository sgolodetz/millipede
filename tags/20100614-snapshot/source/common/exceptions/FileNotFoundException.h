/***
 * millipede: FileNotFoundException.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FILENOTFOUNDEXCEPTION
#define H_MILLIPEDE_FILENOTFOUNDEXCEPTION

#include "Exception.h"

namespace mp {

class FileNotFoundException : public Exception
{
	//#################### CONSTRUCTORS ####################
public:
	explicit FileNotFoundException(const std::string& filename, const std::string& message = "")
	:	Exception("File Not Found: " + filename + ". " + message)
	{}
};

}

#endif
