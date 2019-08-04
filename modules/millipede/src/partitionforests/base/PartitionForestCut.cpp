/***
 * millipede: PartitionForestCut.cpp
 * Copyright Stuart Golodetz, 2018. All rights reserved.
 ***/

#include "partitionforests/base/PartitionForestCut.h"

namespace mp {

//#################### PartitionForestCut - DESTRUCTOR ####################

PartitionForestCut::~PartitionForestCut() {}

//#################### HorizontalCut - CONSTRUCTORS ####################

HorizontalCut::HorizontalCut(int layer)
: m_layer(layer)
{}

//#################### HorizontalCut - PUBLIC METHODS ####################

bool HorizontalCut::contains(const PFNodeID& node) const
{
	return node.layer() == m_layer;
}

int HorizontalCut::max_layer() const
{
	return m_layer;
}

int HorizontalCut::min_layer() const
{
	return m_layer;
}

//#################### NonHorizontalCut - CONSTRUCTORS ####################

NonHorizontalCut::NonHorizontalCut(const std::set<PFNodeID>& cutNodes)
: m_cutNodes(cutNodes), m_maxLayer(INT_MIN), m_minLayer(INT_MAX)
{
	for(std::set<PFNodeID>::const_iterator it = cutNodes.begin(), iend = cutNodes.end(); it != iend; ++it)
	{
		const int layer = it->layer();
		if(layer > m_maxLayer) m_maxLayer = layer;
		if(layer < m_minLayer) m_minLayer = layer;
	}
}

//#################### NonHorizontalCut - PUBLIC METHODS ####################

bool NonHorizontalCut::contains(const PFNodeID& node) const
{
	return m_cutNodes.find(node) != m_cutNodes.end();
}

int NonHorizontalCut::max_layer() const
{
	return m_maxLayer;
}

int NonHorizontalCut::min_layer() const
{
	return m_minLayer;
}

}
