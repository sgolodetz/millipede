/***
 * millipede: CubeFaceDesignator.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CubeFaceDesignator.h"

namespace mp {

template <> CubeFaceDesignator::Enum enum_begin()
{
	return CubeFaceDesignator::FACE_XY;
}

template <> CubeFaceDesignator::Enum enum_end()
{
	return CubeFaceDesignator::COUNT;
}

CubeFaceDesignator::Enum& operator++(CubeFaceDesignator::Enum& e)
{
	e = CubeFaceDesignator::Enum(e + 1);
	return e;
}

}
