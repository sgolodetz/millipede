/***
 * millipede: GlobalNodeTable.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_GLOBALNODETABLE
#define H_MILLIPEDE_GLOBALNODETABLE

#include <cassert>
#include <map>
#include <vector>

#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>

#include "MeshNode.h"

namespace mp {

template <typename Label>
class GlobalNodeTable
{
	//#################### ENUMERATIONS ####################
public:
	enum NodeDesignator
	{
		NODE_001,				///< node at the midpoint of the +z edge emerging from a point
		NODE_010,				///< node at the midpoint of the +y edge emerging from a point
		NODE_011,				///< node at the centre of the +y+z face emerging from a point
		NODE_100,				///< node at the midpoint of the +x edge emerging from a point
		NODE_101,				///< node at the centre of the +x+z face emerging from a point
		NODE_110,				///< node at the centre of the +x+y face emerging from a point
		NODE_111,				///< node at the centre of the +x+y+z cube emerging from a point
		NODE_DESIGNATOR_COUNT
	};

	//#################### TYPEDEFS ####################
private:
	typedef boost::tuples::tuple<int,int,int> IntTriple;
	typedef MeshNode<Label> MeshNodeT;
	typedef std::vector<MeshNodeT> MeshNodeVector;
	typedef boost::shared_ptr<MeshNodeVector> MeshNodeVector_Ptr;
	typedef std::map<IntTriple,int> Subtable;
	typedef Subtable::iterator SubtableIter;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshNodeVector_Ptr m_masterArray;
	Subtable m_subtables[NODE_DESIGNATOR_COUNT];

	//#################### CONSTRUCTORS ####################
public:
	GlobalNodeTable()
	:	m_masterArray(new MeshNodeVector)
	{}

	//#################### PUBLIC OPERATORS ####################
public:
	MeshNodeT& operator()(int n)
	{
		assert(0 <= n && n < static_cast<int>(m_masterArray->size()));
		return (*m_masterArray)[n];
	}

	const MeshNodeT& operator()(int n) const
	{
		assert(0 <= n && n < static_cast<int>(m_masterArray->size()));
		return (*m_masterArray)[n];
	}

	//#################### PUBLIC METHODS ####################
public:
	int find_index(const IntTriple& loc, NodeDesignator nodeDesignator)
	{
		// NYI
		throw 23;
	}

	const MeshNodeVector_Ptr& master_array()
	{
		return m_masterArray;
	}
};

}

#endif
