/***
 * milllipede: Plane.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLLIPEDE_PLANE
#define H_MILLLIPEDE_PLANE

#include "Vector3.h"

namespace mp {

namespace PlaneClassification {

enum Enum
{
	BACK,
	COPLANAR,
	FRONT,
	STRADDLE,	///< entities with extent (e.g. polygons) only
};

}

/**
@brief	Represents planes of the form ax + by + cz - d = 0, i.e. n . x - d = 0.

Datatype invariant: |n| = 1
*/
class Plane
{
	//#################### PRIVATE VARIABLES ####################
private:
	Vector3d m_normal;
	double m_d;

	//#################### CONSTRUCTORS ####################
public:
	Plane(const Vector3d& normal, double d);
	Plane(const Vector3d& normal, const Vector3d& x);

	//#################### PUBLIC METHODS ####################
public:
	PlaneClassification::Enum classify_point(const Vector3d& p) const;
	double distance_to_point(const Vector3d& p) const;
	double distance_value() const;
	const Vector3d& normal() const;

	//#################### PRIVATE METHODS ####################
private:
	void ensure_invariant();
};

}

#endif
