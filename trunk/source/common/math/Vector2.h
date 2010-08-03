/***
 * millipede: Vector2.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VECTOR2
#define H_MILLIPEDE_VECTOR2

#include <cmath>

#include <common/exceptions/Exception.h>
#include "NumericUtil.h"

namespace mp {

/**
@brief	Vector2 is a class template representing 2D vectors with components of type T.

@tparam	T	The type of vector component
*/
template <typename T>
struct Vector2
{
	//#################### PUBLIC VARIABLES ####################
	T x;	///< The x component of the vector
	T y;	///< The y component of the vector

	//#################### CONSTRUCTORS ####################
	/**
	@brief	Constructs the zero vector (0,0)<sup>T</sup>.
	*/
	Vector2()
	:	x(T()), y(T())
	{}

	/**
	@brief	Constructs the vector (x,y)<sup>T</sup>.

	@param[in]	x_	The x component of the new vector
	@param[in]	y_	The y component of the new vector
	*/
	Vector2(T x_, T y_)
	:	x(x_), y(y_)
	{}

	/**
	@brief	Copy constructs a vector from a vector with a different component type.

	@param[in]	rhs		The other vector
	*/
	template <typename U>
	explicit Vector2(const Vector2<U>& rhs)
	:	x(NumericUtil::round_to_nearest<T>(rhs.x)),
		y(NumericUtil::round_to_nearest<T>(rhs.y))
	{}

	//#################### PUBLIC OPERATORS ####################
	/**
	@brief	Returns the i'th component of the vector.

	@param[in]	i	The index of the component to return
	@pre
		-	i == 0 || i == 1
	@return	x if i == 0, or y if i == 1
	*/
	T& operator[](int i)
	{
		return const_cast<T&>(const_cast<const Vector2<T>*>(this)->operator[](i));
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
			default:	throw Exception("Bad component index");
		}
	}

	/**
	@brief	Adds the vector rhs to this vector.

	@param[in]	rhs		The vector to add
	@return	A reference to this vector
	*/
	Vector2& operator+=(const Vector2& rhs)
	{
		x += rhs.x;
		y += rhs.y;
		return *this;
	}

	/**
	@brief	Subtracts the vector rhs from this vector.

	@param[in]	rhs		The vector to subtract
	@return	A reference to this vector
	*/
	Vector2& operator-=(const Vector2& rhs)
	{
		x -= rhs.x;
		y -= rhs.y;
		return *this;
	}

	/**
	@brief	Scales this vector by the specified factor.

	@param[in]	factor	The factor by which the vector will be scaled
	@return	A reference to this vector
	*/
	Vector2& operator*=(double factor)
	{
		x = NumericUtil::round_to_nearest<T>(x * factor);
		y = NumericUtil::round_to_nearest<T>(y * factor);
		return *this;
	}

	/**
	@brief	Multiples this vector component-wise with rhs.

	@param[in]	rhs		The vector with which to multiply
	@return	A reference to this vector
	*/
	Vector2& operator*=(const Vector2& rhs)
	{
		x *= rhs.x;
		y *= rhs.y;
		return *this;
	}

	/**
	@brief	Scales this vector by the inverse of the specified factor.

	@param[in]	factor	The factor by whose inverse the vector will be scaled
	@return	A reference to this vector
	*/
	Vector2& operator/=(double factor)
	{
		x = NumericUtil::round_to_nearest<T>(x / factor);
		y = NumericUtil::round_to_nearest<T>(y / factor);
		return *this;
	}

	/**
	@brief	Divides this vector component-wise by rhs.

	@param[in]	rhs		The vector by which to divide
	@return	A reference to this vector
	*/
	Vector2& operator/=(const Vector2& rhs)
	{
		x = NumericUtil::round_to_nearest<T>(static_cast<double>(x) / rhs.x);
		y = NumericUtil::round_to_nearest<T>(static_cast<double>(y) / rhs.y);
		return *this;
	}

	/**
	@brief	Returns a new vector that points in the opposite direction to this one.

	@return	As described
	*/
	Vector2 operator-() const
	{
		return Vector2(-x, -y);
	}

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Calculates the distance between this vector and rhs.

	@param[in]	rhs		The vector to which to calculate the distance
	@return	The distance
	*/
	double distance(const Vector2& rhs) const
	{
		return sqrt(distance_squared(rhs));
	}

	/**
	@brief	Calculates the square of the distance between this vector and rhs.

	@param[in]	rhs		The vector to which to calculate the square of the distance
	@return	The square of the distance
	*/
	double distance_squared(const Vector2& rhs) const
	{
		double dx = x - rhs.x;
		double dy = y - rhs.y;
		return dx*dx + dy*dy;
	}
};

//#################### GLOBAL OPERATORS ####################
template <typename T>
Vector2<T> operator+(const Vector2<T>& lhs, const Vector2<T>& rhs)
{
	Vector2<T> copy = lhs;
	copy += rhs;
	return copy;
}

template <typename T>
Vector2<T> operator-(const Vector2<T>& lhs, const Vector2<T>& rhs)
{
	Vector2<T> copy = lhs;
	copy -= rhs;
	return copy;
}

template <typename T>
Vector2<T> operator*(double factor, const Vector2<T>& v)
{
	Vector2<T> copy = v;
	copy *= factor;
	return copy;
}

template <typename T>
Vector2<T> operator*(const Vector2<T>& v, double factor)
{
	Vector2<T> copy = v;
	copy *= factor;
	return copy;
}

template <typename T>
Vector2<T> operator*(const Vector2<T>& lhs, const Vector2<T>& rhs)
{
	Vector2<T> copy = lhs;
	copy *= rhs;
	return copy;
}

template <typename T>
Vector2<T> operator/(const Vector2<T>& v, double factor)
{
	Vector2<T> copy = v;
	copy /= factor;
	return copy;
}

template <typename T>
Vector2<T> operator/(const Vector2<T>& lhs, const Vector2<T>& rhs)
{
	Vector2<T> copy = lhs;
	copy /= rhs;
	return copy;
}

template <typename T>
bool operator<(const Vector2<T>& lhs, const Vector2<T>& rhs)
{
	return	lhs.x < rhs.x ||
			(lhs.x == rhs.x && lhs.y < rhs.y);
}

//#################### TYPEDEFS ####################
typedef Vector2<double> Vector2d;
typedef Vector2<int> Vector2i;

}

#endif
