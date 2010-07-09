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

BOOST_AUTO_TEST_CASE(divideequals_test)
{
	Vector3d d(1.0, 2.0, 3.0);
	d /= 2.0;
		BOOST_CHECK_CLOSE(d.x, 0.5, 0.0001);
		BOOST_CHECK_CLOSE(d.y, 1.0, 0.0001);
		BOOST_CHECK_CLOSE(d.z, 1.5, 0.0001);

	Vector3i i(3, 2, 1);
	i /= 2;
		BOOST_CHECK_EQUAL(i.x, 2);	// there is an internal "round to even" occurring
		BOOST_CHECK_EQUAL(i.y, 1);
		BOOST_CHECK_EQUAL(i.z, 0);	// ditto
}

BOOST_AUTO_TEST_CASE(unaryminus_test)
{
	Vector3d d(23.0, 9.0, 84.0);
	d = -d;
		BOOST_CHECK_EQUAL(d.x, -23.0);
		BOOST_CHECK_EQUAL(d.y, -9.0);
		BOOST_CHECK_EQUAL(d.z, -84.0);

	Vector3i i(-24, -12, -18);
	i = -i;
		BOOST_CHECK_EQUAL(i.x, 24);
		BOOST_CHECK_EQUAL(i.y, 12);
		BOOST_CHECK_EQUAL(i.z, 18);
}

BOOST_AUTO_TEST_CASE(anglebetween_test)
{
	Vector3d di(2.0, 0.0, 0.0), dj(0.0, 3.0, 0.0), dk(0.0, 0.0, 4.0), dm(-1.0, 0.0, 0.0);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(di, di), 0.0, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(dj, dj), 0.0, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(dk, dk), 0.0, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(di, dj), MathConstants::PI/2, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(dj, dk), MathConstants::PI/2, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(dk, di), MathConstants::PI/2, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(di, dm), MathConstants::PI, 0.0001);

	Vector3d dv(1.0, 1.0, 0.0), dw(1.0, 1.0, 1.0);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(dv, dw), acos(sqrt(2.0)/sqrt(3.0)), 0.0001);
}

BOOST_AUTO_TEST_CASE(cross_test)
{
	Vector3d di(1.0, 0.0, 0.0), dj(0.0, 1.0, 0.0), dk(0.0, 0.0, 1.0);
	Vector3d diXdj = di.cross(dj), diXdk = di.cross(dk), djXdi = dj.cross(di), djXdk = dj.cross(dk), dkXdi = dk.cross(di), dkXdj = dk.cross(dj);
		BOOST_CHECK_CLOSE(diXdj.length(), 1.0, 0.0001);
		BOOST_CHECK_CLOSE(diXdk.length(), 1.0, 0.0001);
		BOOST_CHECK_CLOSE(djXdi.length(), 1.0, 0.0001);
		BOOST_CHECK_CLOSE(djXdk.length(), 1.0, 0.0001);
		BOOST_CHECK_CLOSE(dkXdi.length(), 1.0, 0.0001);
		BOOST_CHECK_CLOSE(dkXdj.length(), 1.0, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(diXdj, dk), 0.0, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(diXdk, -dj), 0.0, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(djXdi, -dk), 0.0, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(djXdk, di), 0.0, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(dkXdi, dj), 0.0, 0.0001);
		BOOST_CHECK_CLOSE(Vector3d::angle_between(dkXdj, -di), 0.0, 0.0001);
}

// TODO
