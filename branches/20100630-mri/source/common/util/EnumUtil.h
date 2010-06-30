/***
 * millipede: EnumUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ENUMUTIL
#define H_MILLIPEDE_ENUMUTIL

#include <vector>

namespace mp {

template <typename E> E enum_begin();
template <typename E> E enum_end();

template <typename E>
std::vector<E> enum_values()
{
	std::vector<E> ret;
	for(E e=enum_begin<E>(), end=enum_end<E>(); e!=end; ++e)
	{
		ret.push_back(e);
	}
	return ret;
}

}

#endif
