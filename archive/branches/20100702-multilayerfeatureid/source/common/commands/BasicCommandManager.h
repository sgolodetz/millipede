/***
 * millipede: BasicCommandManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_BASICCOMMANDMANAGER
#define H_MILLIPEDE_BASICCOMMANDMANAGER

#include "ICommandManager.h"

namespace mp {

class BasicCommandManager : public ICommandManager
{
	//#################### PUBLIC METHODS ####################
public:
	bool can_redo() const;
	bool can_undo() const;
	void clear_history();
	void redo();
	std::string redo_description() const;
	void undo();
	std::string undo_description() const;
};

}

#endif
