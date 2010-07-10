/***
 * millipede: MeshNode.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHNODE
#define H_MILLIPEDE_MESHNODE

#include <algorithm>
#include <set>

#include <common/exceptions/Exception.h>
#include "SourcedLabel.h"

namespace mp {

template <typename Label>
class MeshNode
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::set<int> m_adjacentNodes;
	Vector3d m_position;
	std::set<SourcedLabel<Label> > m_sourcedLabels;
	bool m_valid;

	//#################### CONSTRUCTORS ####################
public:
	explicit MeshNode(const Vector3d& position)
	:	m_position(position), m_valid(true)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void add_adjacent_node(int n)
	{
		m_adjacentNodes.insert(n);
	}

	void add_sourced_label(Label label, const Vector3i& source)
	{
		m_sourcedLabels.insert(SourcedLabel<Label>(label, source));
	}

	int adjacent_node_count() const
	{
		return static_cast<int>(m_adjacentNodes.size());
	}

	const std::set<int>& adjacent_nodes() const
	{
		return m_adjacentNodes;
	}

	const Vector3i& find_source_of_label(Label label) const
	{
		for(typename std::set<SourcedLabel<Label> >::const_iterator it=m_sourcedLabels.begin(), iend=m_sourcedLabels.end(); it!=iend; ++it)
		{
			if(it->label == label)
			{
				return it->source;
			}
		}
		throw Exception("The mesh node does not contain the specified label");
	}

	bool has_labels(const std::set<Label>& otherLabels) const
	{
		std::set<Label> ourLabels = labels();
		return std::includes(ourLabels.begin(), ourLabels.end(), otherLabels.begin(), otherLabels.end());
	}

	int label_count() const
	{
		return static_cast<int>(m_sourcedLabels.size());
	}

	std::set<Label> labels() const
	{
		std::set<Label> ret;
		for(typename std::set<SourcedLabel<Label> >::const_iterator it=m_sourcedLabels.begin(), iend=m_sourcedLabels.end(); it!=iend; ++it)
		{
			ret.insert(it->label);
		}
		return ret;
	}

	const Vector3d& position() const
	{
		return m_position;
	}

	void remove_adjacent_node(int n)
	{
		m_adjacentNodes.erase(n);
	}

	void set_adjacent_nodes(const std::set<int>& adjacentNodes)
	{
		m_adjacentNodes = adjacentNodes;
	}

	const std::set<SourcedLabel<Label> >& sourced_labels() const
	{
		return m_sourcedLabels;
	}
};

}

#endif
