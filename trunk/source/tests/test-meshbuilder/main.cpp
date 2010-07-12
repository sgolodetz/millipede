/***
 * test-meshbuilder: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/util/ITKImageUtil.h>
#include <common/visualization/MeshBuilder.h>
using namespace mp;

int main()
{
	typedef int Label;
	typedef MeshBuilder<Label> MeshBuilderT;
	typedef boost::shared_ptr<MeshBuilderT> MeshBuilder_Ptr;
#if 1
	Label pixels[] = {
		0,0,0,
		0,0,0,

		1,1,0,
		1,1,0
	};
	MeshBuilderT::LabelImagePointer labelling = ITKImageUtil::make_filled_image<Label>(3, 2, 2, pixels);
#else
	Label pixels[] = {
		0,1,
		0,1,

		2,2,
		3,3
	};
	MeshBuilderT::LabelImagePointer labelling = ITKImageUtil::make_filled_image<Label>(2, 2, 2, pixels);
#endif
	MeshBuilder_Ptr builder(new MeshBuilderT(labelling->GetLargestPossibleRegion().GetSize(), labelling));
	Job::execute_managed(builder);

	typedef Mesh<Label> MeshT;
	typedef boost::shared_ptr<MeshT> Mesh_Ptr;
	Mesh_Ptr mesh = builder->get_mesh();

	return 0;
}
