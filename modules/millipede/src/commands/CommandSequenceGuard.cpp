/***
 * millipede: CommandSequenceGuard.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "commands/CommandSequenceGuard.h"

#include "commands/ICommandManager.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CommandSequenceGuard::CommandSequenceGuard(const ICommandManager_Ptr& manager, const std::string& description)
:	m_manager(manager), m_description(description)
{
	m_manager->begin_command_sequence();
}

//#################### DESTRUCTOR ####################
CommandSequenceGuard::~CommandSequenceGuard()
{
	m_manager->end_command_sequence(m_description);
}

}
