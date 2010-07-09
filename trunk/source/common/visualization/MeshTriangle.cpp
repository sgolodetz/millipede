/***
 * millipede: MeshTriangle.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MeshTriangle.h"

#include <utility>

namespace mp {

//#################### CONSTRUCTORS ####################
MeshTriangle::MeshTriangle(int index0, int index1, int index2, const Vector3d& normal)
:	m_normal(normal)
{
	m_indices[0] = index0;
	m_indices[1] = index1;
	m_indices[2] = index2;
}

//#################### PUBLIC METHODS ####################
void MeshTriangle::flip_winding()
{
	std::swap(m_indices[0], m_indices[1]);
	m_normal = -m_normal;
}

int MeshTriangle::index(int i) const
{
	return m_indices[i];
}

const Vector3d& MeshTriangle::normal() const
{
	return m_normal;
}

}
