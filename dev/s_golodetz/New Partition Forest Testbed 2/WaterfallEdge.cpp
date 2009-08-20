/***
 * millipede: WaterfallEdge.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "WaterfallEdge.h"

namespace mp {

//#################### CONSTRUCTORS ####################
WaterfallEdge::WaterfallEdge(int weight)
:	m_parent(NULL), m_weight(weight)
{}

//#################### DESTRUCTOR ####################
WaterfallEdge::~WaterfallEdge() {}

//#################### PUBLIC METHODS ####################
void WaterfallEdge::add_child(WaterfallEdge_Ptr child)
{
	m_children.push_back(child);
	child->m_parent = this;
}

std::list<WaterfallEdge_Ptr>& WaterfallEdge::children()
{
	return m_children;
}

void WaterfallEdge::merge(const std::list<WaterfallEdge_Ptr>::iterator& edgeIt)
{
	// Inform the client that the edge has been merged.
	merge_hook();

	// Replace this edge with its children in its parent's child list.
	for(std::list<WaterfallEdge_Ptr>::iterator it=m_children.begin(), iend=m_children.end(); it!=iend; ++it)
	{
		(*it)->m_parent = m_parent;
		m_parent->m_children.push_back(*it);
	}
	m_parent->m_children.erase(edgeIt);
}

int WaterfallEdge::weight() const
{
	return m_weight;
}

}
