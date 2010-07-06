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
	typedef AbdominalFeature::Enum Label;
	typedef MeshBuilder<Label> MB;
	MB::LabelImagePointer labelling = ITKImageUtil::make_image<Label>(2, 2, 2);
	MB mb(labelling);
	// TODO
	return 0;
}
