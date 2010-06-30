/***
 * millipede: BasicCommandManager.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "BasicCommandManager.h"

namespace mp {

//#################### PUBLIC METHODS ####################
bool BasicCommandManager::can_redo() const									{ return false; }
bool BasicCommandManager::can_undo() const									{ return false; }
void BasicCommandManager::clear_history()									{}
void BasicCommandManager::redo()											{}
std::string BasicCommandManager::redo_description() const					{ return ""; }
void BasicCommandManager::undo()											{}
std::string BasicCommandManager::undo_description() const					{ return ""; }

}
