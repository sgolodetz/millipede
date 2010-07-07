/***
 * millipede: SourcedLabel.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SOURCEDLABEL
#define H_MILLIPEDE_SOURCEDLABEL

#include <itkVector.h>

namespace mp {

template <typename Label>
struct SourcedLabel
{
	//#################### PUBLIC VARIABLES ####################
	Label label;
	itk::Vector<double,3> source;

	//#################### CONSTRUCTORS ####################
	SourcedLabel(Label label_, const itk::Vector<double,3>& source_)
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
SourcedLabel<Label> make_sourced_label(Label label, const itk::Vector<double,3>& source)
{
	return SourcedLabel<Label>(label, source);
}

}

#endif
