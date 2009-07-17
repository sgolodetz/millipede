#include <iostream>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

int main()
{
	shared_ptr<int> p(new int(23));
	shared_ptr<int> q = p;
	std::cout << *q << std::endl;
	return 0;
}
