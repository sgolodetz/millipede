/***
 * test-ipfgrid: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <common/partitionforests/base/PartitionForestSelection.h>
#include <common/partitionforests/images/SimpleImageBranchLayer.h>
#include <common/partitionforests/images/SimpleImageLeafLayer.h>
#include <common/segmentation/CTIPFBuilder.h>
#include <common/segmentation/IPFGridBuilder.h>
using namespace mp;

void basic_test()
{
	typedef PartitionForest<SimpleImageLeafLayer,SimpleImageBranchLayer> IPF;
	typedef IPFGrid<IPF> IPFGridT;

	itk::Size<3> volumeSize = {{4, 4, 4}};		// the volume as a whole is of size 4x4x4
	itk::Size<3> subvolumeSize = {{2, 2, 1}};	// each forest will be constructed from a sub-volume of size 2x2x1

	int elementCount = 1;
	for(int i=0; i<3; ++i) elementCount *= volumeSize[i] / subvolumeSize[i];

	// Construct a vector containing the correct number of empty forests, but don't bother actually creating them, as that isn't the purpose of this test.
	std::vector<IPFGridT::Element_Ptr> forests(elementCount);

	// Construct the grid of forests.
	IPFGridT ipfg(forests, subvolumeSize, volumeSize);

	// Check that forest indexing works.
	assert(ipfg.element_index_of(2,1,3) == 13);
}

void selection_test()
{
	typedef PartitionForestSelection<SimpleImageLeafLayer,SimpleImageBranchLayer> IPFSelection;
	typedef ForestGrid<IPFSelection> IPFSelectionGridT;

	itk::Size<3> volumeSize = {{4, 4, 4}};		// the volume as a whole is of size 4x4x4
	itk::Size<3> subvolumeSize = {{2, 2, 1}};	// each forest will be constructed from a sub-volume of size 2x2x1

	int elementCount = 1;
	for(int i=0; i<3; ++i) elementCount *= volumeSize[i] / subvolumeSize[i];

	// Construct a vector containing the correct number of empty forest selections, but don't bother actually creating them, as that isn't the purpose of this test.
	std::vector<IPFSelectionGridT::Element_Ptr> selections(elementCount);

	// Construct the grid of forests.
	IPFSelectionGridT ipfsg(selections, subvolumeSize, volumeSize);
}

int main()
{
	basic_test();
	selection_test();
	return 0;
}
