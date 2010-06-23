/***
 * millipede: Command.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "Command.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Command::Command(const std::string& description)
:	m_depth(-1), m_description(description)
{}

//#################### DESTRUCTOR ####################
Command::~Command()
{}

//#################### PUBLIC METHODS ####################
int Command::depth() const
{
	return m_depth;
}

const std::string& Command::description() const
{
	return m_description;
}

void Command::redo()
{
	execute();
}

//#################### PRIVATE METHODS ####################
void Command::set_depth(int depth)
{
	m_depth = depth;
}

}
