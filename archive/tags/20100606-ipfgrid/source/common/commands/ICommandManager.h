/***
 * millipede: ICommandManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ICOMMANDMANAGER
#define H_MILLIPEDE_ICOMMANDMANAGER

#include <boost/shared_ptr.hpp>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class Command> Command_Ptr;

class ICommandManager
{
	//#################### DESTRUCTOR ####################
public:
	virtual ~ICommandManager() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void begin_command_sequence() = 0;
	virtual bool can_redo() const = 0;
	virtual bool can_undo() const = 0;
	virtual void clear_history() = 0;
	virtual void end_command_sequence(const std::string& description) = 0;
	virtual void execute(const Command_Ptr& command) = 0;
	virtual void redo() = 0;
	virtual std::string redo_description() const = 0;
	virtual void undo() = 0;
	virtual std::string undo_description() const = 0;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<ICommandManager> ICommandManager_Ptr;

}

#endif
