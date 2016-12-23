/***
 * millipede: Plane.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "math/Plane.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Plane::Plane(const Vector3d& normal, double d)
:	m_normal(normal), m_d(d)
{
	ensure_invariant();
}

Plane::Plane(const Vector3d& normal, const Vector3d& x)
:	m_normal(normal), m_d(normal.dot(x))
{
	ensure_invariant();
}

//#################### PUBLIC METHODS ####################
PlaneClassification::Enum Plane::classify_point(const Vector3d& p) const
{
	double value = m_normal.dot(p) - m_d;

	if(fabs(value) < MathConstants::EPSILON) return PlaneClassification::COPLANAR;
	else if(value > 0) return PlaneClassification::FRONT;
	else return PlaneClassification::BACK;
}

double Plane::distance_to_point(const Vector3d& p) const
{
	// Note that this equation is valid precisely because the stored normal is unit length.
	return fabs(m_normal.dot(p) - m_d);
}

double Plane::distance_value() const
{
	return m_d;
}

const Vector3d& Plane::normal() const
{
	return m_normal;
}

//#################### PRIVATE METHODS ####################
void Plane::ensure_invariant()
{
	double length = m_normal.length();
	if(length > MathConstants::SMALL_EPSILON)
	{
		m_normal /= length;
		m_d /= length;
	}
	else throw Exception("The plane's normal must be non-zero");
}

}
