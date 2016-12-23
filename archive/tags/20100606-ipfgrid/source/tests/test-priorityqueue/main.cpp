/***
 * test-priorityqueue: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>

#include <common/adts/PriorityQueue.h>
using namespace mp;

int main()
{
	typedef	PriorityQueue<std::string, double, int, std::greater<double> > PQ;
	PQ pq;
	pq.insert("S", 1.0, 23);
	pq.insert("K", 0.9, 13);
	pq.update_key("K", 1.1);
	while(!pq.empty())
	{
		PQ::Element e = pq.top();
		pq.pop();

		std::cout << e.id() << ' ' << e.key() << ' ' << e.data() << '\n';
	}
	return 0;
}
