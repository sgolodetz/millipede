/***
 * millipede: PriorityQueue.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PRIORITYQUEUE
#define H_MILLIPEDE_PRIORITYQUEUE

#include <map>
#include <vector>

#include <common/exceptions/Exception.h>

namespace mp {

template <typename ID, typename Key, typename Data, typename Comp = std::less<Key> >
class PriorityQueue
{
	//#################### NESTED CLASSES ####################
public:
	class Element
	{
	private:
		ID m_id;
		Key m_key;
		Data m_data;

	public:
		Element() {}
		Element(const ID& id, const Key& key, const Data& data) : m_id(id), m_key(key), m_data(data) {}

		Data& data()				{ return m_data; }
		const ID& id() const		{ return m_id; }
		const Key& key() const		{ return m_key; }

		friend class PriorityQueue;
	};

	//#################### TYPEDEFS ####################
private:
	typedef std::map<ID,size_t> Dictionary;		// maps IDs to their current position in the heap
	typedef std::vector<Element> Heap;

	//#################### PRIVATE VARIABLES ####################
private:
	// Datatype Invariant: m_dictionary.size() == m_heap.size()
	Dictionary m_dictionary;
	Heap m_heap;

	//#################### PUBLIC METHODS ####################
public:
	void clear()
	{
		m_dictionary.clear();
		m_heap.swap(Heap());

		ensure_invariant();
	}

	bool contains(ID id) const
	{
		return m_dictionary.find(id) != m_dictionary.end();
	}

	Element& element(ID id)
	{
		return m_heap[m_dictionary[id]];
	}

	bool empty() const
	{
		return m_dictionary.empty();
	}

	void erase(ID id)
	{
		size_t i = m_dictionary[id];
		m_dictionary.erase(id);
		m_heap[i] = m_heap[m_heap.size()-1];
		if(m_heap[i].id() != id)	// assuming the element we were erasing wasn't the last one in the heap, update the dictionary
		{
			m_dictionary[m_heap[i].id()] = i;
		}
		m_heap.pop_back();
		heapify(i);

		ensure_invariant();
	}

	void insert(ID id, const Key& key, const Data& data)
	{
		if(contains(id))
		{
			throw Exception("An element with the specified ID is already in the priority queue");
		}

		size_t i = m_heap.size();
		m_heap.resize(i+1);
		while(i > 0 && Comp()(key, m_heap[parent(i)].key()))
		{
			size_t p = parent(i);
			m_heap[i] = m_heap[p];
			m_dictionary[m_heap[i].id()] = i;
			i = p;
		}
		m_heap[i] = Element(id, key, data);
		m_dictionary[id] = i;

		ensure_invariant();
	}

	void pop()
	{
		erase(m_heap[0].id());
		ensure_invariant();
	}

	Element top()
	{
		return m_heap[0];
	}

	void update_key(ID id, const Key& key)
	{
		size_t i = m_dictionary[id];
		update_key_at(i, key);

		ensure_invariant();
	}

	//#################### PRIVATE METHODS ####################
private:
	void ensure_invariant()
	{
		if(m_dictionary.size() != m_heap.size())
		{
			throw Exception("The operation which just executed invalidated the priority queue");
		}
	}

	void heapify(size_t i)
	{
		bool done = false;
		while(!done)
		{
			size_t L = left(i), R = right(i);
			size_t largest = i;
			if(L < m_heap.size() && Comp()(m_heap[L].key(), m_heap[largest].key()))
				largest = L;
			if(R < m_heap.size() && Comp()(m_heap[R].key(), m_heap[largest].key()))
				largest = R;
			if(largest != i)
			{
				std::swap(m_heap[i], m_heap[largest]);
				m_dictionary[m_heap[i].id()] = i;
				m_dictionary[m_heap[largest].id()] = largest;
				i = largest;
			}
			else done = true;
		}
	}

	inline static size_t left(size_t i)		{ return 2*i + 1; }
	inline static size_t parent(size_t i)	{ return (i+1)/2 - 1; }

	void percolate(size_t i)
	{
		while(i > 0 && Comp()(m_heap[i].key(), m_heap[parent(i)].key()))
		{
			size_t p = parent(i);
			std::swap(m_heap[i], m_heap[p]);
			m_dictionary[m_heap[i].id()] = i;
			m_dictionary[m_heap[p].id()] = p;
			i = p;
		}
	}

	inline static size_t right(size_t i)	{ return 2*i + 2; }

	void update_key_at(size_t i, const Key& key)
	{
		if(Comp()(key, m_heap[i].key()))
		{
			// The key has increased.
			m_heap[i].m_key = key;
			percolate(i);
		}
		else if(Comp()(m_heap[i].key(), key))
		{
			// The key has decreased.
			m_heap[i].m_key = key;
			heapify(i);
		}
	}
};

}

#endif
