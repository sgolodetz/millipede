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
	Label pixels[] = {0,2,2,2,2,1,2,1};
	MeshBuilderT::LabelImagePointer labelling = ITKImageUtil::make_filled_image<Label>(2, 2, 2, pixels);
	MeshBuilder_Ptr builder(new MeshBuilderT(labelling->GetLargestPossibleRegion().GetSize(), labelling));
	Job::execute_managed(builder);
	return 0;
}
