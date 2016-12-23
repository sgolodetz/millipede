/***
 * millipede: SourcedLabel.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SOURCEDLABEL
#define H_MILLIPEDE_SOURCEDLABEL

#include <common/math/Vector3.h>

namespace mp {

template <typename Label>
struct SourcedLabel
{
	//#################### PUBLIC VARIABLES ####################
	Label label;
	Vector3i source;

	//#################### CONSTRUCTORS ####################
	SourcedLabel(Label label_, const Vector3i& source_)
	:	label(label_), source(source_)
	{}

	//#################### PUBLIC OPERATORS ####################
	bool operator<(const SourcedLabel& rhs) const
	{
		return label < rhs.label;
	}
};

}

#endif
