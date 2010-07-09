/***
 * millipede: Vector3.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VECTOR3
#define H_MILLIPEDE_VECTOR3

#include <cmath>

#include <common/util/NumericUtil.h>
#include "MathConstants.h"

namespace mp {

template <typename T>
struct Vector3
{
	//#################### PUBLIC VARIABLES ####################
	T x, y, z;

	//#################### CONSTRUCTORS ####################
	Vector3()
	:	x(T()), y(T()), z(T())
	{}

	Vector3(T x_, T y_, T z_)
	:	x(x_), y(y_), z(z_)
	{}

	//#################### PUBLIC OPERATORS ####################
	Vector3& operator+=(const Vector3& rhs)
	{
		x += rhs.x;
		y += rhs.y;
		z += rhs.z;
		return *this;
	}

	Vector3& operator-=(const Vector3& rhs)
	{
		x -= rhs.x;
		y -= rhs.y;
		z -= rhs.z;
		return *this;
	}

	Vector3& operator*=(double factor)
	{
		x = NumericUtil::round_to_nearest<T>(x * factor);
		y = NumericUtil::round_to_nearest<T>(y * factor);
		z = NumericUtil::round_to_nearest<T>(z * factor);
		return *this;
	}

	Vector3& operator/=(double factor)
	{
		x = NumericUtil::round_to_nearest<T>(x / factor);
		y = NumericUtil::round_to_nearest<T>(y / factor);
		z = NumericUtil::round_to_nearest<T>(z / factor);
		return *this;
	}

	Vector3 operator-() const
	{
		return Vector3(-x, -y, -z);
	}

	//#################### PUBLIC METHODS ####################
	Vector3 cross(const Vector3& rhs) const
	{
		return Vector3(y*rhs.z - z*rhs.y,
					   z*rhs.x - x*rhs.z,
					   x*rhs.y - y*rhs.x);
	}

	double distance(const Vector3& rhs) const
	{
		return sqrt(distance_squared(rhs));
	}

	double distance_squared(const Vector3& rhs) const
	{
		double dx = x - rhs.x;
		double dy = y - rhs.y;
		double dz = z - rhs.z;
		return dx*dx + dy*dy + dz*dz;
	}

	T dot(const Vector3& rhs) const
	{
		return x*rhs.x + y*rhs.y + z*rhs.z;
	}

	double length() const
	{
		return sqrt(length_squared());
	}

	double length_squared() const
	{
		return x*x + y*y + z*z;
	}

	Vector3& negate()
	{
		x = -x;
		y = -y;
		z = -z;
		return *this;
	}

	Vector3& normalize()
	{
		double len = length();
		if(len < MathConstants::SMALL_EPSILON) throw Exception("Unable to normalize vector: too close to zero");
		return (*this) *= 1.0/len;
	}

	Vector3 project_onto(const Vector3& rhs) const
	{
		// (lhs . rhs / |rhs|) * (rhs / |rhs|) = rhs * (lhs . rhs / |rhs|^2)
		return rhs * (dot(rhs) / rhs.length_squared());
	}
};

//#################### GLOBAL OPERATORS ####################
template <typename T>
Vector3<T> operator+(const Vector3<T>& lhs, const Vector3<T>& rhs)
{
	Vector3<T> copy = lhs;
	copy += rhs;
	return copy;
}

template <typename T>
Vector3<T> operator-(const Vector3<T>& lhs, const Vector3<T>& rhs)
{
	Vector3<T> copy = lhs;
	copy -= rhs;
	return copy;
}

template <typename T>
Vector3<T> operator*(double factor, const Vector3<T>& v)
{
	Vector3<T> copy = v;
	copy *= factor;
	return copy;
}

template <typename T>
Vector3<T> operator*(const Vector3<T>& v, double factor)
{
	Vector3<T> copy = v;
	copy *= factor;
	return copy;
}

template <typename T>
Vector3<T> operator/(const Vector3<T>& v, double factor)
{
	Vector3<T> copy = v;
	copy /= factor;
	return copy;
}

template <typename T>
bool operator<(const Vector3<T>& lhs, const Vector3<T>& rhs)
{
	return	lhs.x < rhs.x ||
			(lhs.x == rhs.x && lhs.y < rhs.y) ||
			(lhs.x == rhs.x && lhs.y == rhs.y && lhs.z < rhs.z);
}

//#################### TYPEDEFS ####################
typedef Vector3<double> Vector3d;
typedef Vector3<int> Vector3i;

}

#endif
