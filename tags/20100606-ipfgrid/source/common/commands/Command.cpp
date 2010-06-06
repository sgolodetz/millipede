/***
 * millipede: Command.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "Command.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Command::Command(const std::string& description)
:	m_description(description)
{}

//#################### DESTRUCTOR ####################
Command::~Command()
{}

//#################### PUBLIC METHODS ####################
const std::string& Command::description() const
{
	return m_description;
}

void Command::redo()
{
	execute();
}

}
