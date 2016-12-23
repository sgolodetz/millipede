/***
 * millipede: IPFMultiFeatureSelectionOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFMULTIFEATURESELECTIONOVERLAY
#define H_MILLIPEDE_IPFMULTIFEATURESELECTIONOVERLAY

#include <common/partitionforests/base/PartitionForestSelection.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>
#include "IPFOverlayTools.h"
#include "PartitionOverlay.h"

namespace mp {

class IPFMultiFeatureSelectionOverlay : public PartitionOverlay
{
	//#################### CONSTRUCTORS ####################
public:
	template <typename LeafLayer, typename BranchLayer, typename Feature>
	IPFMultiFeatureSelectionOverlay(const boost::shared_ptr<const VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> >& multiFeatureSelection,
									const SliceLocation& sliceLocation, SliceOrientation sliceOrientation, const std::map<Feature,RGBA32>& colourMap)
	{
		typedef PartitionForestSelection<LeafLayer,BranchLayer> PartitionForestSelectionT;
		typedef boost::shared_ptr<const PartitionForestSelectionT> PartitionForestSelection_CPtr;

		boost::shared_ptr<const VolumeIPF<LeafLayer,BranchLayer> > volumeIPF = multiFeatureSelection->volume_ipf();
		itk::Index<3> sliceBegin, sliceEnd;
		int width, height;
		IPFOverlayTools::calculate_slice_parameters(volumeIPF->volume_size(), sliceLocation, sliceOrientation, sliceBegin, sliceEnd, width, height);

		RGBA32Image::Pointer image = ITKImageUtil::make_image<RGBA32>(width, height);

		std::vector<Feature> featureTypes = enum_values<Feature>();
		for(size_t i=0, size=featureTypes.size(); i<size; ++i)
		{
			if(!multiFeatureSelection->has_selection(featureTypes[i])) continue;
			PartitionForestSelection_CPtr selection = multiFeatureSelection->selection(featureTypes[i]);
			typename std::map<Feature,RGBA32>::const_iterator jt = colourMap.find(featureTypes[i]);
			RGBA32 fillColour = jt != colourMap.end() ? jt->second : ITKImageUtil::make_rgba32(255,0,255,255);
#if 0
			// OUTLINES ONLY
			fillColour[3] = 255;
#endif
			RGBA32 hatchingColour = fillColour;
			hatchingColour[3] = 200;	// set the alpha value to a reasonably high value (fill colours for features tend to be relatively transparent)

			typedef typename PartitionForestSelectionT::NodeConstIterator Iter;
			for(Iter jt=selection->nodes_cbegin(), jend=selection->nodes_cend(); jt!=jend; ++jt)
			{
#if 1
				IPFOverlayTools::draw_node(volumeIPF, *jt, image, sliceBegin, sliceEnd, sliceOrientation, fillColour, boost::none, hatchingColour);
#else
				// OUTLINES ONLY
				IPFOverlayTools::draw_node(volumeIPF, *jt, image, sliceBegin, sliceEnd, sliceOrientation, fillColour, boost::none, boost::none);
#endif
			}
		}

#if 0
		// OUTLINES ONLY
		IPFOverlayTools::draw_boundaries(image, image);
#endif

		set_texture(TextureFactory::create_texture(image));
	}
};

}

#endif
