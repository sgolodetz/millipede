/***
 * millipede: UndoableCommandManager.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "commands/UndoableCommandManager.h"

#include <deque>

#include "commands/SequenceCommand.h"
#include "exceptions/Exception.h"

namespace mp {

//#################### LOCAL CLASSES ####################
class UndoableCommandManager::MarkerCommand : public Command
{
public:
	MarkerCommand() : Command("Marker") {}
	void execute() {}
	void undo() {}
};

//#################### CONSTRUCTORS ####################
UndoableCommandManager::UndoableCommandManager()
:	m_markerCommand(new MarkerCommand)
{}

//#################### PUBLIC METHODS ####################
bool UndoableCommandManager::can_redo() const
{
	return !m_undone.empty();
}

bool UndoableCommandManager::can_undo() const
{
	return !m_done.empty();
}

void UndoableCommandManager::clear_history()
{
	m_done.clear();
	m_undone.clear();
}

void UndoableCommandManager::redo()
{
	if(can_redo())
	{
		Command_Ptr command = m_undone.back();
		m_undone.pop_back();
		command->redo();
		m_done.push_back(command);
	}
}

std::string UndoableCommandManager::redo_description() const
{
	if(can_redo()) return m_undone.back()->description();
	else return "";
}

void UndoableCommandManager::undo()
{
	if(can_undo())
	{
		Command_Ptr command = m_done.back();
		m_done.pop_back();
		command->undo();
		m_undone.push_back(command);
	}
}

std::string UndoableCommandManager::undo_description() const
{
	if(can_undo()) return m_done.back()->description();
	else return "";
}

//#################### PRIVATE METHODS ####################
void UndoableCommandManager::begin_command_sequence_hook()
{
	execute(m_markerCommand);
}

// Precondition: begin_command_sequence() has been called since end_command_sequence() was last called
void UndoableCommandManager::end_command_sequence_hook(const std::string& description)
{
	std::deque<Command_Ptr> sequence;
	for(;;)
	{
		if(m_done.empty()) throw Exception("No command sequence had been started");

		Command_Ptr latest = m_done.back();
		m_done.pop_back();

		if(latest != m_markerCommand) sequence.push_front(latest);
		else break;
	}
	Command_Ptr command(new SequenceCommand(description, sequence));
	set_depth_of_command(command);
	m_done.push_back(command);
}

void UndoableCommandManager::execute_hook(const Command_Ptr& command)
{
	m_done.push_back(command);
	m_undone.clear();
}

}
