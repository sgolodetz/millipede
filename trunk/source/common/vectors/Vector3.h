/***
 * millipede: Vector3.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VECTOR3
#define H_MILLIPEDE_VECTOR3

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
};

//#################### GLOBAL OPERATORS ####################
template <typename T>
Vector3<T> operator-(const Vector3<T>& v)
{
	return Vector3<T>(-v.x, -v.y, -v.z);
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
