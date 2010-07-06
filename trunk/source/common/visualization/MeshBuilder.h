/***
 * millipede: MeshBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHBUILDER
#define H_MILLIPEDE_MESHBUILDER

#include <functional>

#include <itkImage.h>

#include <common/jobs/CompositeJob.h>

namespace mp {

/**
@brief	A MeshBuilder builds a 3D mesh from a 3D label image using the multiple material marching cubes (M3C) algorithm.

@tparam	Label			The type of label to be used
@tparam	PriorityPred	A predicate type defining an ordering over the labels for resolving conflicts that arise during the algorithm
*/
template <typename Label, typename PriorityPred = std::less<Label> >
class MeshBuilder : public CompositeJob
{
	//#################### TYPEDEFS ####################
public:
	typedef itk::Image<Label,3> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Builds a 3D mesh from a 3D label image.

	@param[in]	labelling	An itk::SmartPointer to the label image
	@pre
		-	labelling is non-null
	*/
	explicit MeshBuilder(const LabelImagePointer& labelling)
	{
		// TODO
	}
};

}

#endif
