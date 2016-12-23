/***
 * millipede: NodeLoop.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_NODELOOP
#define H_MILLIPEDE_NODELOOP

#include <set>
#include <vector>

namespace mp {

template <typename Label>
class NodeLoop
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<int> m_indices;
	std::set<Label> m_labels;

	//#################### CONSTRUCTORS ####################
public:
	NodeLoop(const std::vector<int>& indices, const std::set<Label>& labels)
	:	m_indices(indices), m_labels(labels)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int index(int n) const
	{
		return m_indices[n];
	}

	const std::set<Label>& labels() const
	{
		return m_labels;
	}

	int size() const
	{
		return static_cast<int>(m_indices.size());
	}
};

}

#endif
