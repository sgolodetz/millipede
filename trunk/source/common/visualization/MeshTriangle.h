/***
 * millipede: MeshTriangle.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHTRIANGLE
#define H_MILLIPEDE_MESHTRIANGLE

#include <set>

#include <common/math/Vector3.h>

namespace mp {

template <typename Label>
class MeshTriangle
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_indices[3];			///< indices into the global node table
	std::set<Label> m_labels;	///< the labels of the triangle

	//#################### CONSTRUCTORS ####################
public:
	MeshTriangle(int index0, int index1, int index2, const std::set<Label>& labels)
	:	m_labels(labels)
	{
		m_indices[0] = index0;
		m_indices[1] = index1;
		m_indices[2] = index2;
	}

	//#################### PUBLIC METHODS ####################
public:
	void flip_winding()
	{
		std::swap(m_indices[0], m_indices[1]);
	}

	int index(int i) const
	{
		return m_indices[i];
	}

	const std::set<Label>& labels() const
	{
		return m_labels;
	}
};

}

#endif
