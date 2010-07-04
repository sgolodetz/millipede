/***
 * test-disjointsetforest: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <iostream>
#include <string>

#include <cpptest.h>

#include <common/adts/DisjointSetForest.h>
using namespace mp;

struct DSFTestSuite : Test::Suite
{
	DSFTestSuite()
	{
		TEST_ADD(DSFTestSuite::test_add_element)
		TEST_ADD(DSFTestSuite::test_add_elements)
		TEST_ADD(DSFTestSuite::test_find_set)
		TEST_ADD(DSFTestSuite::test_union_sets)
	}

	void test_add_element()
	{
		DisjointSetForest<std::string> dsf;
		dsf.add_element(0, "a");
			TEST_ASSERT(dsf.element_count() == 1);
			TEST_ASSERT(dsf.set_count() == 1);
			TEST_ASSERT(dsf.find_set(0) == 0);
			TEST_ASSERT(dsf.value_of(0) == "a");
		dsf.add_element(2, "c");
			TEST_ASSERT(dsf.element_count() == 2);
			TEST_ASSERT(dsf.set_count() == 2);
			TEST_ASSERT(dsf.find_set(2) == 2);
			TEST_ASSERT(dsf.value_of(2) == "c");
	}

	void test_add_elements()
	{
		DisjointSetForest<std::string> dsf;
		std::map<int,std::string> elements;
		elements[23] = "s";
		elements[9] = "m";
		elements[84] = "g";
		dsf.add_elements(elements);
			TEST_ASSERT(dsf.element_count() == 3);
			TEST_ASSERT(dsf.set_count() == 3);
			TEST_ASSERT(dsf.find_set(23) == 23);
			TEST_ASSERT(dsf.find_set(9) == 9);
			TEST_ASSERT(dsf.find_set(84) == 84);
			TEST_ASSERT(dsf.value_of(23) == "s");
			TEST_ASSERT(dsf.value_of(9) == "m");
			TEST_ASSERT(dsf.value_of(84) == "g");
	}

	void test_find_set()
	{
		DisjointSetForest<std::string> dsf;
			TEST_THROWS(dsf.find_set(17), Exception);
			TEST_THROWS(dsf.find_set(10), Exception);
		dsf.add_element(17, "s");
			TEST_ASSERT(dsf.find_set(17) == 17);
			TEST_THROWS(dsf.find_set(10), Exception);
		dsf.add_element(10, "g");
			TEST_ASSERT(dsf.find_set(17) == 17);
			TEST_ASSERT(dsf.find_set(10) == 10);
	}

	void test_union_sets()
	{
		DisjointSetForest<std::string> dsf;
		dsf.add_element(23, "s");
		dsf.add_element(9, "m");
		dsf.add_element(84, "g");
			TEST_ASSERT(dsf.element_count() == 3);
			TEST_ASSERT(dsf.set_count() == 3);
			TEST_ASSERT(dsf.find_set(23) == 23);
			TEST_ASSERT(dsf.find_set(9) == 9);
			TEST_ASSERT(dsf.find_set(84) == 84);
		dsf.union_sets(23, 84);
			TEST_ASSERT(dsf.element_count() == 3);
			TEST_ASSERT(dsf.set_count() == 2);
			TEST_ASSERT(dsf.find_set(23) == dsf.find_set(84));
			TEST_ASSERT(dsf.find_set(23) == 23 || dsf.find_set(23) == 84);
			TEST_ASSERT(dsf.find_set(9) == 9);
			TEST_ASSERT(dsf.value_of(23) == "s");
			TEST_ASSERT(dsf.value_of(9) == "m");
			TEST_ASSERT(dsf.value_of(84) == "g");
		dsf.union_sets(84, 9);
			TEST_ASSERT(dsf.element_count() == 3);
			TEST_ASSERT(dsf.set_count() == 1);
			TEST_ASSERT(dsf.find_set(9) == dsf.find_set(84) && dsf.find_set(84) == dsf.find_set(23));
			TEST_ASSERT(dsf.find_set(9) == 9 || dsf.find_set(9) == 84 || dsf.find_set(9) == 23);
			TEST_ASSERT(dsf.value_of(23) == "s");
			TEST_ASSERT(dsf.value_of(9) == "m");
			TEST_ASSERT(dsf.value_of(84) == "g");
	}
};

int main()
{
	DSFTestSuite suite;
	Test::TextOutput output(Test::TextOutput::Verbose);
	suite.run(output, true);
	return 0;
}
