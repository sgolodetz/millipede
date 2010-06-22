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
	//#################### PRIVATE VARIABLES ####################
private:
	int m_commandDepth;

	//#################### CONSTRUCTORS ####################
public:
	ICommandManager();

	//#################### DESTRUCTOR ####################
public:
	virtual ~ICommandManager();

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual bool can_redo() const = 0;
	virtual bool can_undo() const = 0;
	virtual void clear_history() = 0;
	virtual void redo() = 0;
	virtual std::string redo_description() const = 0;
	virtual void undo() = 0;
	virtual std::string undo_description() const = 0;

	//#################### PRIVATE ABSTRACT METHODS ####################
private:
	virtual void begin_command_sequence_hook()								{}
	virtual void end_command_sequence_hook(const std::string& description)	{}
	virtual void execute_hook(const Command_Ptr& command)					{}

	//#################### PUBLIC METHODS ####################
public:
	void begin_command_sequence();
	int command_depth() const;
	void end_command_sequence(const std::string& description);
	void execute(const Command_Ptr& command);

	//#################### PROTECTED METHODS ####################
protected:
	void set_depth_of_command(const Command_Ptr& command);
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<ICommandManager> ICommandManager_Ptr;
typedef boost::shared_ptr<const ICommandManager> ICommandManager_CPtr;

}

#endif
