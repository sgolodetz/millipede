/***
 * millipede: CubeFaceDesignator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBEFACEDESIGNATOR
#define H_MILLIPEDE_CUBEFACEDESIGNATOR

#include <common/util/EnumUtil.h>

namespace mp {

namespace CubeFaceDesignator {

enum Enum
{
	FACE_XY,
	FACE_XZ,
	FACE_YZ,
	COUNT,
};

}

template <> CubeFaceDesignator::Enum enum_begin();
template <> CubeFaceDesignator::Enum enum_end();
CubeFaceDesignator::Enum& operator++(CubeFaceDesignator::Enum& e);

}

#endif
