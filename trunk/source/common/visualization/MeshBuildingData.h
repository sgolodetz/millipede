/***
 * millipede: MeshBuildingData.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHBUILDINGDATA
#define H_MILLIPEDE_MESHBUILDINGDATA

#include <itkImage.h>

namespace mp {

template <typename Label>
struct MeshBuildingData
{
	//#################### TYPEDEFS ####################
	typedef itk::Image<Label,3> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;

	//#################### CONSTRUCTORS ####################
	explicit MeshBuildingData(const LabelImagePointer& labelling_)
	:	labelling(labelling_)
	{}

	//#################### PUBLIC VARIABLES ####################
	LabelImagePointer labelling;
	// TODO: Cube face table
	// TODO: Node map
	// TODO: Triangles
};

}

#endif
