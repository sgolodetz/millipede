/***
 * millipede: DataHook.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DATAHOOK
#define H_MILLIPEDE_DATAHOOK

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>

namespace mp {

/**
@brief	A data hook is an indirect holder of data.

It is useful when you know that your data must come from a particular source, but that source can't produce it yet.
For instance, when implementing a job pipeline, there might be a producer/consumer relationship between jobs A and B.
B requires data from A to function, but at the point at which the pipeline is being set up, A cannot yet run, and its
output (the input to B) is not available. Data hooks provide a solution to the problem: all that is necessary is to give
A an output hook and B an input hook, and then connect the two together (by setting one equal to the other). A then sets
its output using the output hook's set() method, and B later retrieves it using its input hook's get().

@tparam	T	The type of data the hook should contain (arbitrary)
*/
template <typename T>
class DataHook
{
	//#################### PRIVATE VARIABLES ####################
private:
	boost::shared_ptr<boost::optional<T> > m_data;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Constructs an empty data hook.
	*/
	DataHook()
	:	m_data(new boost::optional<T>)
	{}

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Returns whether the hook is currently empty (i.e. has not yet been set).

	@return	true, if the hook is empty, or false if it isn't
	*/
	bool empty() const
	{
		return *m_data != boost::none;
	}

	/**
	@brief	Gets the contents of the data hook.

	@pre
		-	set() must have been called beforehand
	@return	As described
	*/
	T& get()
	{
		return **m_data;
	}

	/**
	@brief	Gets the contents of the data hook.

	@pre
		-	set() must have been called beforehand
	@return	As described
	*/
	const T& get() const
	{
		return **m_data;
	}

	/**
	@brief	Sets the contents of the data hook.

	@param[in]	data	The new contents of the hook
	*/
	void set(const T& data)
	{
		*m_data = data;
	}
};

}

#endif
