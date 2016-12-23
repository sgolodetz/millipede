/***
 * millipede: BasicCommandManager.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "BasicCommandManager.h"

#include "Command.h"

namespace mp {

//#################### PUBLIC METHODS ####################
void BasicCommandManager::begin_command_sequence()					{}
bool BasicCommandManager::can_redo() const							{ return false; }
bool BasicCommandManager::can_undo() const							{ return false; }
void BasicCommandManager::clear_history()							{}
void BasicCommandManager::end_command_sequence(const std::string&)	{}
void BasicCommandManager::execute(const Command_Ptr& command)		{ command->execute(); }
void BasicCommandManager::redo()									{}
std::string BasicCommandManager::redo_description() const			{ return ""; }
void BasicCommandManager::undo()									{}
std::string BasicCommandManager::undo_description() const			{ return ""; }

}
