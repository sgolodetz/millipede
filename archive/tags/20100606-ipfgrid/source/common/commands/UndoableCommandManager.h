/***
 * millipede: UndoableCommandManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_UNDOABLECOMMANDMANAGER
#define H_MILLIPEDE_UNDOABLECOMMANDMANAGER

#include <vector>

#include "ICommandManager.h"

namespace mp {

class UndoableCommandManager : public ICommandManager
{
	//#################### NESTED CLASSES ####################
private:
	class MarkerCommand;

	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<Command_Ptr> m_done;
	std::vector<Command_Ptr> m_undone;

	Command_Ptr m_markerCommand;

	//#################### CONSTRUCTORS ####################
public:
	UndoableCommandManager();

	//#################### PUBLIC METHODS ####################
public:
	void begin_command_sequence();
	bool can_redo() const;
	bool can_undo() const;
	void clear_history();
	void end_command_sequence(const std::string& description);
	void execute(const Command_Ptr& command);
	void redo();
	std::string redo_description() const;
	void undo();
	std::string undo_description() const;
};

}

#endif
