/***
 * millipede: Vector3.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VECTOR3
#define H_MILLIPEDE_VECTOR3

#include <cmath>

#include <common/exceptions/Exception.h>
#include "MathConstants.h"
#include "NumericUtil.h"

namespace mp {

/**
@brief	Vector3 is a class template representing 3D vectors with components of type T.

@tparam	T	The type of vector component
*/
template <typename T>
struct Vector3
{
	//#################### PUBLIC VARIABLES ####################
	T x;	///< The x component of the vector
	T y;	///< The y component of the vector
	T z;	///< The z component of the vector

	//#################### CONSTRUCTORS ####################
	/**
	@brief	Constructs the zero vector (0,0,0)<sup>T</sup>.
	*/
	Vector3()
	:	x(T()), y(T()), z(T())
	{}

	/**
	@brief	Constructs the vector (x,y,z)<sup>T</sup>.

	@param[in]	x_	The x component of the new vector
	@param[in]	y_	The y component of the new vector
	@param[in]	z_	The z component of the new vector
	*/
	Vector3(T x_, T y_, T z_)
	:	x(x_), y(y_), z(z_)
	{}

	/**
	@brief	Copy constructs a vector from a vector with a different component type.

	@param[in]	rhs		The other vector
	*/
	template <typename U>
	explicit Vector3(const Vector3<U>& rhs)
	:	x(NumericUtil::round_to_nearest<T>(rhs.x)),
		y(NumericUtil::round_to_nearest<T>(rhs.y)),
		z(NumericUtil::round_to_nearest<T>(rhs.z))
	{}

	//#################### PUBLIC OPERATORS ####################
	/**
	@brief	Returns the i'th component of the vector.

	@param[in]	i	The index of the component to return
	@pre
		-	0 <= i <= 2
	@return	x if i == 0, y if i == 1, or z if i == 2
	*/
	T& operator[](int i)
	{
		return const_cast<T&>(const_cast<const Vector3<T>*>(this)->operator[](i));
	}

	/**
	@brief	The const version of operator[].
	*/
	const T& operator[](int i) const
	{
		switch(i)
		{
			case 0:		return x;
			case 1:		return y;
			case 2:		return z;
			default:	throw Exception("Bad component index");
		}
	}

	/**
	@brief	Adds the vector rhs to this vector.

	@param[in]	rhs		The vector to add
	@return	A reference to this vector
	*/
	Vector3& operator+=(const Vector3& rhs)
	{
		x += rhs.x;
		y += rhs.y;
		z += rhs.z;
		return *this;
	}

	/**
	@brief	Subtracts the vector rhs from this vector.

	@param[in]	rhs		The vector to subtract
	@return	A reference to this vector
	*/
	Vector3& operator-=(const Vector3& rhs)
	{
		x -= rhs.x;
		y -= rhs.y;
		z -= rhs.z;
		return *this;
	}

	/**
	@brief	Scales this vector by the specified factor.

	@param[in]	factor	The factor by which the vector will be scaled
	@return	A reference to this vector
	*/
	Vector3& operator*=(double factor)
	{
		x = NumericUtil::round_to_nearest<T>(x * factor);
		y = NumericUtil::round_to_nearest<T>(y * factor);
		z = NumericUtil::round_to_nearest<T>(z * factor);
		return *this;
	}

	/**
	@brief	Scales this vector by the inverse of the specified factor.

	@param[in]	factor	The factor by whose inverse the vector will be scaled
	@return	A reference to this vector
	*/
	Vector3& operator/=(double factor)
	{
		x = NumericUtil::round_to_nearest<T>(x / factor);
		y = NumericUtil::round_to_nearest<T>(y / factor);
		z = NumericUtil::round_to_nearest<T>(z / factor);
		return *this;
	}

	/**
	@brief	Returns a new vector that points in the opposite direction to this one.

	@return	As described
	*/
	Vector3 operator-() const
	{
		return Vector3(-x, -y, -z);
	}

	//#################### PUBLIC METHODS ####################
	/**
	@brief	Calculates the angle (in radians) between the vectors lhs and rhs.

	@param[in]	lhs		The first vector
	@param[in]	rhs		The second vector
	@pre
		-	lhs.length() >= MathConstants::SMALL_EPSILON
		-	rhs.length() >= MathConstants::SMALL_EPSILON
	@return	The angle, in the range [0,pi]
	@throw Exception
		-	If the preconditions are violated
	*/
	static double angle_between(const Vector3& lhs, const Vector3& rhs)
	{
		if(lhs.length_squared() < MathConstants::SMALL_EPSILON || rhs.length_squared() < MathConstants::SMALL_EPSILON)
		{
			throw Exception("Unable to determine angle between vectors: at least one of the vectors is too close to the zero vector");
		}

		double cosAngle = lhs.dot(rhs) / (lhs.length() * rhs.length());
		if(cosAngle < -1.0) cosAngle = -1.0;
		if(cosAngle > 1.0) cosAngle = 1.0;
		return acos(cosAngle);
	}

	/**
	@brief	Calculates the cross product *this x rhs.

	@param[in]	rhs		The vector on the right-hand side of the cross product
	@return	The resulting vector
	*/
	Vector3 cross(const Vector3& rhs) const
	{
		return Vector3(y*rhs.z - z*rhs.y,
					   z*rhs.x - x*rhs.z,
					   x*rhs.y - y*rhs.x);
	}

	/**
	@brief	Calculates the distance between this vector and rhs.

	@param[in]	rhs		The vector to which to calculate the distance
	@return	The distance
	*/
	double distance(const Vector3& rhs) const
	{
		return sqrt(distance_squared(rhs));
	}

	/**
	@brief	Calculates the square of the distance between this vector and rhs.

	@param[in]	rhs		The vector to which to calculate the square of the distance
	@return	The square of the distance
	*/
	double distance_squared(const Vector3& rhs) const
	{
		double dx = x - rhs.x;
		double dy = y - rhs.y;
		double dz = z - rhs.z;
		return dx*dx + dy*dy + dz*dz;
	}

	/**
	@brief	Calculates the dot product of this vector and rhs.

	@param[in]	rhs		The vector with which to calculate the dot product
	@return	The result of the dot product
	*/
	T dot(const Vector3& rhs) const
	{
		return x*rhs.x + y*rhs.y + z*rhs.z;
	}

	/**
	@brief	Returns the length of this vector.

	@return	As described
	*/
	double length() const
	{
		return sqrt(length_squared());
	}

	/**
	@brief	Returns the square of the length of this vector.

	@return	As described
	*/
	double length_squared() const
	{
		return x*x + y*y + z*z;
	}

	/**
	@brief	Negates this vector in-place, i.e. individually negates each of its components.

	@return	A reference to this vector (post-negation)
	*/
	Vector3& negate()
	{
		x = -x;
		y = -y;
		z = -z;
		return *this;
	}

	/**
	@brief	Normalizes this vector in-place.

	@return	A reference to this vector (post-normalization)
	*/
	Vector3& normalize()
	{
		double len = length();
		if(len < MathConstants::SMALL_EPSILON) throw Exception("Unable to normalize vector: too close to zero");
		return (*this) *= 1.0/len;
	}

	/**
	@brief	Returns the projection of this vector onto rhs.

	@param[in]	rhs		The vector onto which to calculate the projection
	@return	As described
	*/
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
