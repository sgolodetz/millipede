/***
 * millipede: Iterator.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ITERATOR
#define H_MILLIPEDE_ITERATOR

namespace mp {

template <typename T>
class Iterator
{
	//#################### DESTRUCTOR ####################
public:
	virtual ~Iterator() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual bool has_next() const = 0;
	virtual T next() = 0;
};
}

#endif
