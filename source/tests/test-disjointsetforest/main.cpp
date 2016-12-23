/***
 * test-disjointsetforest: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#define BOOST_TEST_MODULE DisjointSetForest Test
#include <boost/test/included/unit_test.hpp>

#include <common/adts/DisjointSetForest.h>
using namespace mp;

BOOST_AUTO_TEST_CASE(add_element_test)
{
	DisjointSetForest<std::string> dsf;
	dsf.add_element(0, "a");
		BOOST_CHECK_EQUAL(dsf.element_count(), 1);
		BOOST_CHECK_EQUAL(dsf.set_count(), 1);
		BOOST_CHECK_EQUAL(dsf.find_set(0), 0);
		BOOST_CHECK_EQUAL(dsf.value_of(0), "a");
	dsf.add_element(2, "c");
		BOOST_CHECK_EQUAL(dsf.element_count(), 2);
		BOOST_CHECK_EQUAL(dsf.set_count(), 2);
		BOOST_CHECK_EQUAL(dsf.find_set(2), 2);
		BOOST_CHECK_EQUAL(dsf.value_of(2), "c");
}

BOOST_AUTO_TEST_CASE(add_elements_test)
{
	DisjointSetForest<std::string> dsf;
	std::map<int,std::string> elements;
	elements[23] = "s";
	elements[9] = "m";
	elements[84] = "g";
	dsf.add_elements(elements);
		BOOST_CHECK_EQUAL(dsf.element_count(), 3);
		BOOST_CHECK_EQUAL(dsf.set_count(), 3);
		BOOST_CHECK_EQUAL(dsf.find_set(23), 23);
		BOOST_CHECK_EQUAL(dsf.find_set(9), 9);
		BOOST_CHECK_EQUAL(dsf.find_set(84), 84);
		BOOST_CHECK_EQUAL(dsf.value_of(23), "s");
		BOOST_CHECK_EQUAL(dsf.value_of(9), "m");
		BOOST_CHECK_EQUAL(dsf.value_of(84), "g");
}

BOOST_AUTO_TEST_CASE(find_set_test)
{
	DisjointSetForest<std::string> dsf;
		BOOST_CHECK_THROW(dsf.find_set(17), Exception);
		BOOST_CHECK_THROW(dsf.find_set(10), Exception);
	dsf.add_element(17, "s");
		BOOST_CHECK_EQUAL(dsf.find_set(17), 17);
		BOOST_CHECK_THROW(dsf.find_set(10), Exception);
	dsf.add_element(10, "g");
		BOOST_CHECK_EQUAL(dsf.find_set(17), 17);
		BOOST_CHECK_EQUAL(dsf.find_set(10), 10);
}

BOOST_AUTO_TEST_CASE(union_sets_test)
{
	DisjointSetForest<std::string> dsf;
	dsf.add_element(23, "s");
	dsf.add_element(9, "m");
	dsf.add_element(84, "g");
		BOOST_CHECK_EQUAL(dsf.element_count(), 3);
		BOOST_CHECK_EQUAL(dsf.set_count(), 3);
		BOOST_CHECK_EQUAL(dsf.find_set(23), 23);
		BOOST_CHECK_EQUAL(dsf.find_set(9), 9);
		BOOST_CHECK_EQUAL(dsf.find_set(84), 84);
	dsf.union_sets(23, 84);
		BOOST_CHECK_EQUAL(dsf.element_count(), 3);
		BOOST_CHECK_EQUAL(dsf.set_count(), 2);
		BOOST_CHECK_EQUAL(dsf.find_set(23), dsf.find_set(84));
		BOOST_CHECK(dsf.find_set(23) == 23 || dsf.find_set(23) == 84);
		BOOST_CHECK_EQUAL(dsf.find_set(9), 9);
		BOOST_CHECK_EQUAL(dsf.value_of(23), "s");
		BOOST_CHECK_EQUAL(dsf.value_of(9), "m");
		BOOST_CHECK_EQUAL(dsf.value_of(84), "g");
	dsf.union_sets(84, 9);
		BOOST_CHECK_EQUAL(dsf.element_count(), 3);
		BOOST_CHECK_EQUAL(dsf.set_count(), 1);
		BOOST_CHECK(dsf.find_set(9) == dsf.find_set(84) && dsf.find_set(84) == dsf.find_set(23));
		BOOST_CHECK(dsf.find_set(9) == 9 || dsf.find_set(9) == 84 || dsf.find_set(9) == 23);
		BOOST_CHECK_EQUAL(dsf.value_of(23), "s");
		BOOST_CHECK_EQUAL(dsf.value_of(9), "m");
		BOOST_CHECK_EQUAL(dsf.value_of(84), "g");
}
