/***
 * millipede: CompositeListenerBase.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_COMPOSITELISTENERBASE
#define H_MILLIPEDE_COMPOSITELISTENERBASE

#include <list>

#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>

namespace mp {

template <typename ListenerType>
class CompositeListenerBase : public ListenerType
{
	//#################### TYPEDEFS ####################
private:
	typedef std::list<boost::weak_ptr<ListenerType> > Listeners;

	//#################### PRIVATE VARIABLES ####################
private:
	Listeners m_listeners;

	//#################### PUBLIC METHODS ####################
public:
	void add_listener(const boost::shared_ptr<ListenerType>& listener)
	{
		m_listeners.push_back(listener);
	}

	//#################### PROTECTED METHODS ####################
protected:
	template <typename Func>
	void multicast(Func func)
	{
		for(typename Listeners::iterator it=m_listeners.begin(), iend=m_listeners.end(); it!=iend;)
		{
			boost::shared_ptr<ListenerType> listener = it->lock();
			if(listener)	{ func(*listener); ++it; }
			else			{ it = m_listeners.erase(it); }
		}
	}
};

}

#endif
