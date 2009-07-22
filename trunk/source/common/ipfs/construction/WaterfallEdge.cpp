/***
 * millipede: WaterfallEdge.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "WaterfallEdge.h"

namespace mp {

//#################### CONSTRUCTORS ####################
WaterfallEdge::WaterfallEdge(int weight)
:	m_weight(weight)
{}

//#################### DESTRUCTOR ####################
WaterfallEdge::~WaterfallEdge() {}

//#################### PUBLIC METHODS ####################
void WaterfallEdge::add_child(WaterfallEdge_CPtr child)
{
	m_children.push_back(child);
}

const std::list<WaterfallEdge_CPtr>& WaterfallEdge::children() const
{
	return m_children;
}

void WaterfallEdge::merge() const
{
	merge_hook();
}

int WaterfallEdge::weight() const
{
	return m_weight;
}

}
