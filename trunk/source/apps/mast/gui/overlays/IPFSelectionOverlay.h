/***
 * millipede: IPFSelectionOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFSELECTIONOVERLAY
#define H_MILLIPEDE_IPFSELECTIONOVERLAY

#include <common/partitionforests/images/VolumeIPFSelection.h>
#include <common/slices/SliceLocation.h>
#include <common/slices/SliceOrientation.h>
#include <common/textures/RGBA32ImageTexture.h>
#include <common/util/ITKImageUtil.h>
#include "PartitionOverlay.h"

namespace mp {

class IPFSelectionOverlay : public PartitionOverlay
{
	template <typename VolumeIPF_CPtr>
	void draw_node(const VolumeIPF_CPtr& volumeIPF, const PFNodeID& node, RGBA32Image::Pointer image, const itk::Index<3>& begin, const itk::Index<3>& end,
				   SliceOrientation sliceOrientation, const RGBA32& colour, bool boundariesOnly)
	{
		std::list<int> receptiveRegion = volumeIPF->receptive_region_of(node);
		for(std::list<int>::const_iterator it=receptiveRegion.begin(), iend=receptiveRegion.end(); it!=iend; ++it)
		{
			// Calculate the position of the leaf.
			itk::Index<3> volumePos = volumeIPF->position_of_leaf(*it);

			// Ignore the leaf if it's not within [begin,end).
			if(volumePos[0] < begin[0] || volumePos[1] < begin[1] || volumePos[2] < begin[2] ||
			   volumePos[0] >= end[0] || volumePos[1] >= end[1] || volumePos[2] >= end[2])
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

	//#################### CONSTRUCTORS ####################
public:
	template <typename LeafLayer, typename BranchLayer>
	explicit IPFSelectionOverlay(const boost::shared_ptr<const VolumeIPFSelection<LeafLayer,BranchLayer> >& selection,
								 const SliceLocation& sliceLocation, SliceOrientation sliceOrientation)
	{
		boost::shared_ptr<const VolumeIPF<LeafLayer,BranchLayer> > volumeIPF = selection->volume_ipf();
		itk::Size<3> volumeSize = volumeIPF->volume_size();
		itk::Index<3> begin = {{0,0,0}};
		itk::Index<3> end = ITKImageUtil::make_index_from_size(volumeSize);
		int width = -1, height = -1;
		switch(sliceOrientation)
		{
			case ORIENT_XY:
				begin[2] = sliceLocation.z;
				end[2] = sliceLocation.z + 1;
				width = volumeSize[0], height = volumeSize[1];
				break;
			case ORIENT_XZ:
				begin[1] = sliceLocation.y;
				end[1] = sliceLocation.y + 1;
				width = volumeSize[0], height = volumeSize[2];
				break;
			case ORIENT_YZ:
				begin[0] = sliceLocation.x;
				end[0] = sliceLocation.x + 1;
				width = volumeSize[1], height = volumeSize[2];
				break;
		}

		RGBA32Image::Pointer image = ITKImageUtil::make_image<RGBA32>(width, height);

		RGBA32 fc, bc;	// fill and boundary colours
		fc[0] = 255, fc[1] = 0, fc[2] = 0, fc[3] = 50;
		bc[0] = 255, bc[1] = 0, bc[2] = 0, bc[3] = 255;

		typedef typename VolumeIPFSelection<LeafLayer,BranchLayer>::ViewNodeConstIterator Iter;
		for(Iter it=selection->view_at_layer_cbegin(sliceLocation.layer), iend=selection->view_at_layer_cend(sliceLocation.layer); it!=iend; ++it)
		{
			draw_node(volumeIPF, *it, image, begin, end, sliceOrientation, fc, false);
			draw_node(volumeIPF, *it, image, begin, end, sliceOrientation, bc, true);
		}

		set_texture(TextureFactory::create_texture(image));
	}

	//#################### PUBLIC METHODS ####################
public:
	std::string name() const
	{
		return "IPFSelection";
	}
};

}

#endif
