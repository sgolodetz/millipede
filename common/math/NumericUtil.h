/***
 * millipede: NumericUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_NUMERICUTIL
#define H_MILLIPEDE_NUMERICUTIL

#include <boost/numeric/conversion/converter.hpp>

namespace mp {

namespace NumericUtil {

template <typename Target, typename Source>
Target round_to_nearest(Source s)
{
	using namespace boost::numeric;
	typedef converter<Target,Source,conversion_traits<Target,Source>,silent_overflow_handler,RoundEven<Source> > Conv;
	return Conv::convert(s);
}

}

}

#endif
