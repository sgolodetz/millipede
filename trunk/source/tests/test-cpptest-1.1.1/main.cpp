/***
 * test-cpptest-1.1.1: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <cpptest.h>

struct SomeTestSuite : Test::Suite
{
	SomeTestSuite()
	{
		TEST_ADD(SomeTestSuite::test)
	}

	void test()
	{
		TEST_FAIL("This will always fail")
	}
};

int main()
{
	SomeTestSuite sts;
	Test::TextOutput output(Test::TextOutput::Verbose);
	sts.run(output);
	return 0;
}
