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
class ListenerAlertingCommandSequenceGuard
{
	//#################### COMMANDS ####################
private:
	struct PreCommand : Command
	{
		boost::shared_ptr<Listener> listener;
		std::string description;
		int commandDepth;

		explicit PreCommand(const boost::shared_ptr<Listener>& listener_, const std::string& description_, int commandDepth_)
		:	Command("Listener Alerting Pre-Command"), listener(listener_), description(description_), commandDepth(commandDepth_)
		{}

		void execute()	{ listener->command_sequence_execution_began(description, commandDepth); }
		void undo()		{ listener->command_sequence_undo_ended(description, commandDepth); }
	};

	struct PostCommand : Command
	{
		boost::shared_ptr<Listener> listener;
		std::string description;
		int commandDepth;

		explicit PostCommand(const boost::shared_ptr<Listener>& listener_, const std::string& description_, int commandDepth_)
		:	Command("Listener Alerting Post-Command"), listener(listener_), description(description_), commandDepth(commandDepth_)
		{}

		void execute()	{ listener->command_sequence_execution_ended(description, commandDepth); }
		void undo()		{ listener->command_sequence_undo_began(description, commandDepth); }
	};

	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_description;
	boost::shared_ptr<Listener> m_listener;
	ICommandManager_Ptr m_manager;

	//#################### CONSTRUCTORS ####################
public:
	ListenerAlertingCommandSequenceGuard(const ICommandManager_Ptr& manager, const boost::shared_ptr<Listener>& listener, const std::string& description)
	:	m_description(description), m_listener(listener), m_manager(manager)
	{
		m_manager->begin_command_sequence();
		m_manager->execute(Command_Ptr(new PreCommand(m_listener, m_description, m_manager->command_depth() - 1)));
	}

	//#################### DESTRUCTOR ####################
public:
	~ListenerAlertingCommandSequenceGuard()
	{
		m_manager->execute(Command_Ptr(new PostCommand(m_listener, m_description, m_manager->command_depth() - 1)));
		m_manager->end_command_sequence(m_description);
	}

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
private:
	ListenerAlertingCommandSequenceGuard(const ListenerAlertingCommandSequenceGuard&);
	ListenerAlertingCommandSequenceGuard& operator=(const ListenerAlertingCommandSequenceGuard&);
};

}

#endif
