/***
 * millipede: SourcedLabel.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SOURCEDLABEL
#define H_MILLIPEDE_SOURCEDLABEL

#include <common/vectors/Vector3.h>

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

//#################### TEMPLATE FUNCTIONS ####################
template <typename Label>
SourcedLabel<Label> make_sourced_label(Label label, const Vector3i& source)
{
	return SourcedLabel<Label>(label, source);
}

}

#endif
