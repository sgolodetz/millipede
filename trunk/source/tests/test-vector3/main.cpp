/***
 * test-vector3: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#define BOOST_TEST_MODULE Vector3 Test
#include <boost/test/included/unit_test.hpp>

#include <common/math/Vector3.h>
using namespace mp;

BOOST_AUTO_TEST_CASE(constructor_test)
{
	Vector3d d1;
		BOOST_CHECK_EQUAL(d1.x, 0.0);
		BOOST_CHECK_EQUAL(d1.y, 0.0);
		BOOST_CHECK_EQUAL(d1.z, 0.0);

	Vector3d d2(1.0, 2.0, 3.0);
		BOOST_CHECK_EQUAL(d2.x, 1.0);
		BOOST_CHECK_EQUAL(d2.y, 2.0);
		BOOST_CHECK_EQUAL(d2.z, 3.0);

	Vector3i i1;
		BOOST_CHECK_EQUAL(i1.x, 0);
		BOOST_CHECK_EQUAL(i1.y, 0);
		BOOST_CHECK_EQUAL(i1.z, 0);

	Vector3i i2(2, 4, 6);
		BOOST_CHECK_EQUAL(i2.x, 2);
		BOOST_CHECK_EQUAL(i2.y, 4);
		BOOST_CHECK_EQUAL(i2.z, 6);
}

BOOST_AUTO_TEST_CASE(plusequals_test)
{
	Vector3d d1(1.0, 2.0, 3.0);
	Vector3d d2(2.0, 4.0, 6.0);
	d1 += d2;
		BOOST_CHECK_CLOSE(d1.x, 3.0, 0.0001);
		BOOST_CHECK_CLOSE(d1.y, 6.0, 0.0001);
		BOOST_CHECK_CLOSE(d1.z, 9.0, 0.0001);

	Vector3i i1(3, 2, 1);
	Vector3i i2(6, 4, 2);
	i1 += i2;
		BOOST_CHECK_EQUAL(i1.x, 9);
		BOOST_CHECK_EQUAL(i1.y, 6);
		BOOST_CHECK_EQUAL(i1.z, 3);
}

BOOST_AUTO_TEST_CASE(minusequals_test)
{
	Vector3d d1(1.0, 2.0, 3.0);
	Vector3d d2(2.0, 4.0, 6.0);
	d1 -= d2;
		BOOST_CHECK_CLOSE(d1.x, -1.0, 0.0001);
		BOOST_CHECK_CLOSE(d1.y, -2.0, 0.0001);
		BOOST_CHECK_CLOSE(d1.z, -3.0, 0.0001);

	Vector3i i1(3, 2, 1);
	Vector3i i2(6, 4, 2);
	i1 -= i2;
		BOOST_CHECK_EQUAL(i1.x, -3);
		BOOST_CHECK_EQUAL(i1.y, -2);
		BOOST_CHECK_EQUAL(i1.z, -1);
}

BOOST_AUTO_TEST_CASE(timesequals_test)
{
	Vector3d d(1.0, 2.0, 3.0);
	d *= 2.0;
		BOOST_CHECK_CLOSE(d.x, 2.0, 0.0001);
		BOOST_CHECK_CLOSE(d.y, 4.0, 0.0001);
		BOOST_CHECK_CLOSE(d.z, 6.0, 0.0001);

	Vector3i i(3, 2, 1);
	i *= 2;
		BOOST_CHECK_EQUAL(i.x, 6);
		BOOST_CHECK_EQUAL(i.y, 4);
		BOOST_CHECK_EQUAL(i.z, 2);
	i *= 1.5;
		BOOST_CHECK_EQUAL(i.x, 9);
		BOOST_CHECK_EQUAL(i.y, 6);
		BOOST_CHECK_EQUAL(i.z, 3);
}

// TODO
