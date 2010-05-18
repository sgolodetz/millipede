/***
 * millipede: PFNodeID.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "PFNodeID.h"

#include <ostream>

namespace mp {

//#################### CONSTRUCTORS ####################
PFNodeID::PFNodeID(int layer, int index)
:	m_layer(layer), m_index(index)
{}

//#################### PUBLIC METHODS ####################
int PFNodeID::index() const
{
	return m_index;
}

int PFNodeID::layer() const
{
	return m_layer;
}

bool PFNodeID::operator==(const PFNodeID& rhs) const
{
	return m_layer == rhs.m_layer && m_index == rhs.m_index;
}

bool PFNodeID::operator!=(const PFNodeID& rhs) const
{
	return !(*this == rhs);
}

bool PFNodeID::operator<(const PFNodeID& rhs) const
{
	return m_layer < rhs.m_layer || (m_layer == rhs.m_layer && m_index < rhs.m_index);
}

PFNodeID PFNodeID::invalid()
{
	return PFNodeID(-1, -1);
}

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const PFNodeID& rhs)
{
	os << '(' << rhs.layer() << ',' << rhs.index() << ')';
	return os;
}

}