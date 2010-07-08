/***
 * millipede: CubeFace.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CubeFace.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CubeFace::CubeFace()
{
	for(int n=0; n<POTENTIAL_NODE_COUNT; ++n)
	{
		m_localToGlobalNodeMap[n] = UNUSED;
	}
}

//#################### PUBLIC METHODS ####################
int CubeFace::global_node_index(NodeDesignator n) const
{
	return m_localToGlobalNodeMap[n];
}

bool CubeFace::is_used(NodeDesignator n) const
{
	return m_localToGlobalNodeMap[n] != UNUSED;
}

void CubeFace::set_global_node_index(NodeDesignator n, int index)
{
	m_localToGlobalNodeMap[n] = index;
}

void CubeFace::set_used(NodeDesignator n)
{
	m_localToGlobalNodeMap[n] = USED;
}

}
