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
	typedef std::list<ListenerType*> RawListeners;
	typedef std::list<boost::shared_ptr<ListenerType> > SharedListeners;
	typedef std::list<boost::weak_ptr<ListenerType> > WeakListeners;

	//#################### PRIVATE VARIABLES ####################
private:
	RawListeners m_rawListeners;
	SharedListeners m_sharedListeners;
	WeakListeners m_weakListeners;

	//#################### PUBLIC METHODS ####################
public:
	void add_raw_listener(ListenerType *listener)
	{
		m_rawListeners.push_back(listener);
	}

	void add_shared_listener(const boost::shared_ptr<ListenerType>& listener)
	{
		m_sharedListeners.push_back(listener);
	}

	void add_weak_listener(const boost::weak_ptr<ListenerType>& listener)
	{
		m_weakListeners.push_back(listener);
	}

	//#################### PROTECTED METHODS ####################
protected:
	template <typename Func>
	void multicast(Func func)
	{
		for(typename RawListeners::iterator it=m_rawListeners.begin(), iend=m_rawListeners.end(); it!=iend; ++it)
		{
			func(**it);
		}

		for(typename SharedListeners::iterator it=m_sharedListeners.begin(), iend=m_sharedListeners.end(); it!=iend; ++it)
		{
			func(**it);
		}

		for(typename WeakListeners::iterator it=m_weakListeners.begin(), iend=m_weakListeners.end(); it!=iend;)
		{
			boost::shared_ptr<ListenerType> listener = it->lock();
			if(listener)	{ func(*listener); ++it; }
			else			{ it = m_weakListeners.erase(it); }
		}
	}
};

}

#endif
