/***
 * millipede: ICommandManager.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "commands/ICommandManager.h"

#include "commands/Command.h"
#include "exceptions/Exception.h"

namespace mp {

//#################### CONSTRUCTORS ####################
ICommandManager::ICommandManager()
:	m_commandDepth(0)
{}

//#################### DESTRUCTOR ####################
ICommandManager::~ICommandManager()
{}

//#################### PUBLIC METHODS ####################
void ICommandManager::begin_command_sequence()
{
	++m_commandDepth;
	begin_command_sequence_hook();
}

int ICommandManager::command_depth() const
{
	return m_commandDepth;
}

void ICommandManager::end_command_sequence(const std::string& description)
{
	if(m_commandDepth == 0) throw Exception("Command depth would be > 0 if a command sequence had been started");
	--m_commandDepth;
	end_command_sequence_hook(description);
}

void ICommandManager::execute(const Command_Ptr& command)
{
	set_depth_of_command(command);
	command->execute();
	execute_hook(command);
}

//#################### PROTECTED METHODS ####################
void ICommandManager::set_depth_of_command(const Command_Ptr& command)
{
	command->set_depth(m_commandDepth);
}

}
