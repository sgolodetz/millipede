/***
 * test-boost_1_39_0: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <iostream>

#include <boost/shared_ptr.hpp>
#include <boost/thread/condition.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/thread.hpp>

//#################### SHARED_PTR TEST ####################
void test_shared_ptr()
{
	boost::shared_ptr<int> p(new int(23));
	boost::shared_ptr<int> q = p;
	std::cout << *q << std::endl;
}

//#################### THREADS TEST ####################
boost::mutex mut;
boost::condition cond;
bool ready = false;

void thread_a()
{
	for(int i=0; i<5; ++i) std::cout << "In Thread A\n";

	boost::mutex::scoped_lock lock(mut);
	ready = true;
	cond.notify_one();
}

void thread_b()
{
	boost::mutex::scoped_lock lock(mut);
	if(!ready) cond.wait(lock);

	for(int i=0; i<5; ++i) std::cout << "In Thread B\n";
}

void test_threads()
{
	boost::thread thA(thread_a);
	boost::thread thB(thread_b);
	thA.join();
	thB.join();
}

//#################### MAIN FUNCTION ####################
int main()
{
	test_shared_ptr();
	test_threads();
	return 0;
}
