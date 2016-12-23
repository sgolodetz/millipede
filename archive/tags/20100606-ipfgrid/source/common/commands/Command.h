/***
 * millipede: Command.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_COMMAND
#define H_MILLIPEDE_COMMAND

#include <string>

namespace mp {

class Command
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_description;

	//#################### CONSTRUCTORS ####################
public:
	explicit Command(const std::string& description);

	//#################### DESTRUCTOR ####################
public:
	virtual ~Command();

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void execute() = 0;
	virtual void undo() = 0;

	//#################### PUBLIC METHODS ####################
public:
	const std::string& description() const;

	// Note:	Sometimes there may be ways of redoing a command that are more efficient than simply re-executing it.
	//			This hook method is provided to let individual commands override their redo() when this is the case.
	virtual void redo();
};

}

#endif
