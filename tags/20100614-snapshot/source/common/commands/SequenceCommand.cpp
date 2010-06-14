/***
 * millipede: SequenceCommand.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SequenceCommand.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SequenceCommand::SequenceCommand(const std::string& description, const std::deque<Command_Ptr>& commands)
:	Command(description), m_commands(commands)
{}

//#################### PUBLIC METHODS ####################
void SequenceCommand::execute()
{
	for(std::deque<Command_Ptr>::const_iterator it=m_commands.begin(), iend=m_commands.end(); it!=iend; ++it)
	{
		(*it)->execute();
	}
}

void SequenceCommand::redo()
{
	for(std::deque<Command_Ptr>::const_iterator it=m_commands.begin(), iend=m_commands.end(); it!=iend; ++it)
	{
		(*it)->redo();
	}
}

void SequenceCommand::undo()
{
	for(std::deque<Command_Ptr>::const_reverse_iterator it=m_commands.rbegin(), iend=m_commands.rend(); it!=iend; ++it)
	{
		(*it)->undo();
	}
}

}
