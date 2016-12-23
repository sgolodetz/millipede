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

#include "MeshNode.h"

namespace mp {

template <typename Label>
class GlobalNodeTable
{
	//#################### ENUMERATIONS ####################
public:
	enum NodeOffset
	{
		OFFSET_001,				///< offset to the midpoint of the +z edge emerging from a point
		OFFSET_010,				///< offset to the midpoint of the +y edge emerging from a point
		OFFSET_011,				///< offset to the centre of the +y+z face emerging from a point
		OFFSET_100,				///< offset to the midpoint of the +x edge emerging from a point
		OFFSET_101,				///< offset to the centre of the +x+z face emerging from a point
		OFFSET_110,				///< offset to the centre of the +x+y face emerging from a point
		OFFSET_111,				///< offset to the centre of the +x+y+z cube emerging from a point
		OFFSET_COUNT
	};

	//#################### NESTED CLASSES ####################
public:
	class NodePosition
	{
		friend class GlobalNodeTable<Label>;

	private:
		Vector3i base;
		NodeOffset offset;

	public:
		NodePosition()
		{}

		NodePosition(const Vector3i& base_, NodeOffset offset_)
		:	base(base_), offset(offset_)
		{}
	};

	//#################### TYPEDEFS ####################
private:
	typedef MeshNode<Label> MeshNodeT;
	typedef std::vector<MeshNodeT> MeshNodeVector;
	typedef boost::shared_ptr<MeshNodeVector> MeshNodeVector_Ptr;
	typedef boost::shared_ptr<const MeshNodeVector> MeshNodeVector_CPtr;
	typedef std::map<Vector3i,int> Subtable;
	typedef Subtable::iterator SubtableIter;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshNodeVector_Ptr m_masterArray;
	Subtable m_subtables[OFFSET_COUNT];

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
	/**
	@brief	Finds the index of the global node at the specified position. If the node doesn't already exist
			in the master array, it is created.

	Global node positions are specified as (base,offset), where base is a point in the volume and
	offset specifies an offset from base in one or more of the positive axis directions. For example,
	the node ((1,0,0),OFFSET_011) would be the node at the centre of the face between the voxel
	(0,0,0)-(1,1,1)	and the voxel (1,0,0)-(2,1,1), since OFFSET_011 means "add (0,0.5,0.5) to base"
	to get the real position. In particular, this means that ((1,0,0),OFFSET_011)'s position is
	(1,0.5,0.5).

	@param[in]	pos		The position of the node
	@return	As described
	*/
	int find_index(const NodePosition& pos)
	{
		const Vector3i& base = pos.base;
		NodeOffset offset = pos.offset;

		if(offset == OFFSET_COUNT)
		{
			throw Exception("Invalid node offset");
		}

		SubtableIter it = m_subtables[offset].find(base);
		if(it != m_subtables[offset].end())
		{
			return it->second;
		}
		else
		{
			int index = static_cast<int>(m_masterArray->size());
			switch(offset)
			{
			case OFFSET_001:
				m_masterArray->push_back(make_node(base.x, base.y, base.z + 0.5));
				break;
			case OFFSET_010:
				m_masterArray->push_back(make_node(base.x, base.y + 0.5, base.z));
				break;
			case OFFSET_011:
				m_masterArray->push_back(make_node(base.x, base.y + 0.5, base.z + 0.5));
				break;
			case OFFSET_100:
				m_masterArray->push_back(make_node(base.x + 0.5, base.y, base.z));
				break;
			case OFFSET_101:
				m_masterArray->push_back(make_node(base.x + 0.5, base.y, base.z + 0.5));
				break;
			case OFFSET_110:
				m_masterArray->push_back(make_node(base.x + 0.5, base.y + 0.5, base.z));
				break;
			case OFFSET_111:
				m_masterArray->push_back(make_node(base.x + 0.5, base.y + 0.5, base.z + 0.5));
				break;
			default:
				// This will never happen: offset != OFFSET_COUNT
				break;
			}
			m_subtables[offset].insert(std::make_pair(base, index));
			return index;
		}
	}

	const MeshNodeVector_Ptr& master_array()
	{
		return m_masterArray;
	}

	MeshNodeVector_CPtr master_array() const
	{
		return m_masterArray;
	}

	//#################### PRIVATE METHODS ####################
private:
	static MeshNodeT make_node(double x, double y, double z)
	{
		return MeshNodeT(Vector3d(x,y,z));
	}
};

}

#endif
