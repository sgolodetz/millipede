/***
 * millipede: IPFOverlayTools.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFOVERLAYTOOLS
#define H_MILLIPEDE_IPFOVERLAYTOOLS

#include <common/partitionforests/images/VolumeIPF.h>
#include <common/slices/SliceLocation.h>
#include <common/slices/SliceOrientation.h>
#include <common/textures/RGBA32ImageTexture.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

namespace IPFOverlayTools {

//#################### FUNCTIONS ####################
void calculate_slice_parameters(const itk::Size<3>& volumeSize, const SliceLocation& sliceLocation, SliceOrientation sliceOrientation,
								itk::Index<3>& sliceBegin, itk::Index<3>& sliceEnd, int& width, int& height);

//#################### TEMPLATE FUNCTIONS ####################
template <typename VolumeIPF_CPtr>
void draw_node(const VolumeIPF_CPtr& volumeIPF, const PFNodeID& node, RGBA32Image::Pointer image,
			   const itk::Index<3>& sliceBegin, const itk::Index<3>& sliceEnd, SliceOrientation sliceOrientation,
			   const RGBA32& colour, bool boundariesOnly)
{
	std::list<int> receptiveRegion = volumeIPF->receptive_region_of(node);
	for(std::list<int>::const_iterator it=receptiveRegion.begin(), iend=receptiveRegion.end(); it!=iend; ++it)
	{
		// Calculate the position of the leaf.
		itk::Index<3> volumePos = volumeIPF->position_of_leaf(*it);

		// Ignore the leaf if it's not within [sliceBegin,sliceEnd).
		if(volumePos[0] < sliceBegin[0] || volumePos[1] < sliceBegin[1] || volumePos[2] < sliceBegin[2] ||
		   volumePos[0] >= sliceEnd[0] || volumePos[1] >= sliceEnd[1] || volumePos[2] >= sliceEnd[2])
		{
			continue;
		}

		if(boundariesOnly)
		{
			bool boundary = false;
			std::vector<itk::Offset<3> > offsets = ITKImageUtil::make_4_connected_offsets(sliceOrientation);
			for(size_t i=0, size=offsets.size(); i<size; ++i)
			{
				if(volumeIPF->node_of(node.layer(), volumePos + offsets[i]) != node)
				{
					boundary = true;
					break;
				}
			}
			if(!boundary) continue;
		}

		// Project the position into image coordinates.
		itk::Index<2> imagePos;
		switch(sliceOrientation)
		{
			case ORIENT_XY:	imagePos[0] = volumePos[0]; imagePos[1] = volumePos[1]; break;
			case ORIENT_XZ:	imagePos[0] = volumePos[0]; imagePos[1] = volumePos[2]; break;
			case ORIENT_YZ:	imagePos[0] = volumePos[1]; imagePos[1] = volumePos[2]; break;
		}

		// Draw the pixel.
		image->SetPixel(imagePos, colour);
	}
}

}

}

#endif
