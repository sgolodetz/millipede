/***
 * test-ipfgrid: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <common/partitionforests/images/SimpleImageBranchLayer.h>
#include <common/partitionforests/images/SimpleImageLeafLayer.h>
#include <common/segmentation/CTIPFBuilder.h>
#include <common/segmentation/IPFGridBuilder.h>
using namespace mp;

void basic_test()
{
	typedef IPFGrid<SimpleImageLeafLayer,SimpleImageBranchLayer> IPFG;

	// The volume as a whole is of size 4x4x4.
	itk::Size<3> volumeSize;
	volumeSize.Fill(4);

	// Each forest will be constructed from a sub-volume of size 2x2x1.
	itk::Size<3> subvolumeSize;
	subvolumeSize[0] = 2;
	subvolumeSize[1] = 2;
	subvolumeSize[2] = 1;

	int forestCount = 1;
	for(int i=0; i<3; ++i) forestCount *= volumeSize[i] / subvolumeSize[i];

	// Construct a vector containing the correct number of empty forests, but don't bother actually creating them, as that isn't the purpose of this test.
	std::vector<IPFG::IPF_Ptr> forests(forestCount);

	// Construct the grid of forests.
	IPFG ipfg(forests, subvolumeSize, volumeSize);

	// Check that forest indexing works.
	assert(ipfg.forest_index_of(2,1,3) == 13);
}

void segmentation_test()
{
	typedef IPFGridBuilder<CTIPFBuilder> CTIPFGridBuilder;
	// TODO
}

int main()
{
	basic_test();
	//segmentation_test();
	return 0;
}
