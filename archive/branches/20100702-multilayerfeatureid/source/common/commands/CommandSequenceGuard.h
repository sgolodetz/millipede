/***
 * millipede: CommandSequenceGuard.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_COMMANDSEQUENCEGUARD
#define H_MILLIPEDE_COMMANDSEQUENCEGUARD

#include <string>

#include <boost/shared_ptr.hpp>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class ICommandManager> ICommandManager_Ptr;

class CommandSequenceGuard
{
	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_manager;
	std::string m_description;

	//#################### CONSTRUCTORS ####################
public:
	CommandSequenceGuard(const ICommandManager_Ptr& manager, const std::string& description);

	//#################### DESTRUCTOR ####################
public:
	~CommandSequenceGuard();

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
private:
	CommandSequenceGuard(const CommandSequenceGuard&);
	CommandSequenceGuard& operator=(const CommandSequenceGuard&);
};

}

#endif
