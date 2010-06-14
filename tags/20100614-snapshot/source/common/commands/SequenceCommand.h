/***
 * millipede: SequenceCommand.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEQUENCECOMMAND
#define H_MILLIPEDE_SEQUENCECOMMAND

#include <deque>

#include <boost/shared_ptr.hpp>

#include "Command.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<Command> Command_Ptr;

class SequenceCommand : public Command
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::deque<Command_Ptr> m_commands;

	//#################### CONSTRUCTORS ####################
public:
	SequenceCommand(const std::string& description, const std::deque<Command_Ptr>& commands);

	//#################### PUBLIC METHODS ####################
public:
	void execute();
	void redo();
	void undo();
};

}

#endif
