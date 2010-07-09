/***
 * millipede: MeshTriangle.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHTRIANGLE
#define H_MILLIPEDE_MESHTRIANGLE

#include <common/vectors/Vector3.h>

namespace mp {

class MeshTriangle
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_indices[3];	///< indices into the global node table
	Vector3d m_normal;	///< the normal of the triangle (assuming ccw on front face)

	//#################### CONSTRUCTORS ####################
public:
	MeshTriangle(int index0, int index1, int index2, const Vector3d& normal);

	//#################### PUBLIC METHODS ####################
public:
	void flip_winding();
	int index(int i) const;
	const Vector3d& normal() const;
};

}

#endif
