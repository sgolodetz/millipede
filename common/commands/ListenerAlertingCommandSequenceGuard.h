/***
 * millipede: ListenerAlertingCommandSequenceGuard.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_LISTENERALERTINGCOMMANDSEQUENCEGUARD
#define H_MILLIPEDE_LISTENERALERTINGCOMMANDSEQUENCEGUARD

#include <string>

#include <boost/shared_ptr.hpp>

#include "Command.h"
#include "ICommandManager.h"

namespace mp {

template <typename Listener>
struct PreSequenceCommand : Command
{
	boost::shared_ptr<Listener> listener;
	std::string description;
	int commandDepth;

	explicit PreSequenceCommand(const boost::shared_ptr<Listener>& listener_, const std::string& description_, int commandDepth_)
	:	Command("Pre-Sequence Command"), listener(listener_), description(description_), commandDepth(commandDepth_)
	{}

	void execute()	{ listener->command_sequence_execution_began(description, commandDepth); }
	void undo()		{ listener->command_sequence_undo_ended(description, commandDepth); }
};

template <typename Listener>
struct PostSequenceCommand : Command
{
	boost::shared_ptr<Listener> listener;
	std::string description;
	int commandDepth;

	explicit PostSequenceCommand(const boost::shared_ptr<Listener>& listener_, const std::string& description_, int commandDepth_)
	:	Command("Post-Sequence Command"), listener(listener_), description(description_), commandDepth(commandDepth_)
	{}

	void execute()	{ listener->command_sequence_execution_ended(description, commandDepth); }
	void undo()		{ listener->command_sequence_undo_began(description, commandDepth); }
};

template <typename Listener1>
class ListenerAlertingCommandSequenceGuard
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_description;
	boost::shared_ptr<Listener1> m_listener1;
	ICommandManager_Ptr m_manager;

	//#################### CONSTRUCTORS ####################
public:
	ListenerAlertingCommandSequenceGuard(const ICommandManager_Ptr& manager, const std::string& description, const boost::shared_ptr<Listener1>& listener1)
	:	m_description(description), m_listener1(listener1), m_manager(manager)
	{
		m_manager->begin_command_sequence();
		m_manager->execute(Command_Ptr(new PreSequenceCommand<Listener1>(m_listener1, m_description, m_manager->command_depth() - 1)));
	}

	//#################### DESTRUCTOR ####################
public:
	~ListenerAlertingCommandSequenceGuard()
	{
		m_manager->execute(Command_Ptr(new PostSequenceCommand<Listener1>(m_listener1, m_description, m_manager->command_depth() - 1)));
		m_manager->end_command_sequence(m_description);
	}

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
private:
	ListenerAlertingCommandSequenceGuard(const ListenerAlertingCommandSequenceGuard&);
	ListenerAlertingCommandSequenceGuard& operator=(const ListenerAlertingCommandSequenceGuard&);
};

template <typename Listener1, typename Listener2>
class ListenerAlertingCommandSequenceGuard2
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_description;
	boost::shared_ptr<Listener1> m_listener1;
	boost::shared_ptr<Listener2> m_listener2;
	ICommandManager_Ptr m_manager;

	//#################### CONSTRUCTORS ####################
public:
	ListenerAlertingCommandSequenceGuard2(const ICommandManager_Ptr& manager, const std::string& description,
										  const boost::shared_ptr<Listener1>& listener1,
										  const boost::shared_ptr<Listener2>& listener2)
	:	m_description(description), m_listener1(listener1), m_listener2(listener2), m_manager(manager)
	{
		m_manager->begin_command_sequence();
		m_manager->execute(Command_Ptr(new PreSequenceCommand<Listener1>(m_listener1, m_description, m_manager->command_depth() - 1)));
		m_manager->execute(Command_Ptr(new PreSequenceCommand<Listener2>(m_listener2, m_description, m_manager->command_depth() - 1)));
	}

	//#################### DESTRUCTOR ####################
public:
	~ListenerAlertingCommandSequenceGuard2()
	{
		m_manager->execute(Command_Ptr(new PostSequenceCommand<Listener2>(m_listener2, m_description, m_manager->command_depth() - 1)));
		m_manager->execute(Command_Ptr(new PostSequenceCommand<Listener1>(m_listener1, m_description, m_manager->command_depth() - 1)));
		m_manager->end_command_sequence(m_description);
	}

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
private:
	ListenerAlertingCommandSequenceGuard2(const ListenerAlertingCommandSequenceGuard2&);
	ListenerAlertingCommandSequenceGuard2& operator=(const ListenerAlertingCommandSequenceGuard2&);
};

}

#endif
