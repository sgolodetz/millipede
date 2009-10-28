/***
 * millipede: Map.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MAP
#define H_MILLIPEDE_MAP

#include <map>

#include <boost/optional.hpp>

namespace mp {

using boost::optional;

template <typename Key, typename Value, typename Pred = std::less<Key>, typename Alloc = std::allocator<std::pair<const Key,Value> > >
class Map
{
	//#################### TYPEDEFS ####################
private:
	typedef std::map<Key,Value,Pred,Alloc> BaseMap;

	//#################### PRIVATE VARIABLES ####################
private:
	BaseMap m_base;

	//#################### PUBLIC OPERATORS ####################
public:
	Value& operator[](const Key& key)
	{
		return m_base[key];
	}

	//#################### PUBLIC METHODS ####################
public:
	BaseMap& base()
	{
		return m_base;
	}

	const BaseMap& base() const
	{
		return m_base;
	}

	void clear()
	{
		m_base.clear();
	}

	bool contains(const Key& key) const
	{
		return m_base.find(key) != m_base.end();
	}

	bool empty() const
	{
		return m_base.empty();
	}

	void erase(const Key& key)
	{
		m_base.erase(key);
	}

	optional<Value&> get(const Key& key)
	{
		BaseMap::iterator it = m_base.find(key);
		if(it != m_base.end()) return it->second;
		else return boost::none;
	}

	optional<const Value&> get(const Key& key) const
	{
		BaseMap::const_iterator it = m_base.find(key);
		if(it != m_base.end()) return it->second;
		else return boost::none;
	}

	bool insert(const Key& key, const Value& value)
	{
		return m_base.insert(std::make_pair(key, value)).second;
	}

	void set(const Key& key, const Value& value)
	{
		BaseMap::iterator it = m_base.find(key);
		if(it != m_base.end()) it->second = value;
		else m_base.insert(std::make_pair(key, value));
	}

	size_t size() const
	{
		return m_base.size();
	}
};

}

#endif
