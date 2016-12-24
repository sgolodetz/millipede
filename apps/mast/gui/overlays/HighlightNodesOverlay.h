/***
 * millipede: HighlightNodesOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_HIGHLIGHTNODESOVERLAY
#define H_MILLIPEDE_HIGHLIGHTNODESOVERLAY

#include <utility>

#include <millipede/partitionforests/images/VolumeIPF.h>
#include "IPFOverlayTools.h"
#include "PartitionOverlay.h"

namespace mp {

//#################### TYPEDEFS ####################
typedef std::pair<std::set<PFNodeID>,RGBA32> NodeHighlightSet;
typedef std::vector<NodeHighlightSet> NodeHighlightSets;

class HighlightNodesOverlay : public PartitionOverlay
{
	//#################### CONSTRUCTORS ####################
public:
	template <typename LeafLayer, typename BranchLayer>
	HighlightNodesOverlay(const NodeHighlightSets& nodeHighlightSets, const boost::shared_ptr<const VolumeIPF<LeafLayer,BranchLayer> >& volumeIPF,
						  const SliceLocation& sliceLocation, SliceOrientation sliceOrientation)
	{
		itk::Index<3> sliceBegin, sliceEnd;
		int width, height;
		IPFOverlayTools::calculate_slice_parameters(volumeIPF->volume_size(), sliceLocation, sliceOrientation, sliceBegin, sliceEnd, width, height);

		RGBA32Image::Pointer image = ITKImageUtil::make_image<RGBA32>(width, height);

		for(NodeHighlightSets::const_iterator it=nodeHighlightSets.begin(), iend=nodeHighlightSets.end(); it!=iend; ++it)
		{
			const std::set<PFNodeID>& nodes = it->first;
			const RGBA32& colour = it->second;
			for(std::set<PFNodeID>::const_iterator jt=nodes.begin(), jend=nodes.end(); jt!=jend; ++jt)
			{
				// Only draw the node if we're viewing the layer it's in.
				if(jt->layer() == sliceLocation.layer)
				{
					IPFOverlayTools::draw_node(volumeIPF, *jt, image, sliceBegin, sliceEnd, sliceOrientation, colour, boost::none, boost::none);
				}
			}
		}

		set_texture(TextureFactory::create_texture(image));
	}

	//#################### PUBLIC METHODS ####################
public:
	bool on_dicom_canvas() const
	{
		return false;
	}
};

}

#endif
