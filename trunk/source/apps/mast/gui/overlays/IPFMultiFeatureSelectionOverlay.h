/***
 * millipede: IPFMultiFeatureSelectionOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFMULTIFEATURESELECTIONOVERLAY
#define H_MILLIPEDE_IPFMULTIFEATURESELECTIONOVERLAY

#include <common/adts/Map.h>
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
									const SliceLocation& sliceLocation, SliceOrientation sliceOrientation, const Map<Feature,RGBA32>& colourMap)
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
			PartitionForestSelection_CPtr selection = multiFeatureSelection->selection(featureTypes[i]);
			RGBA32 colour = colourMap.contains(featureTypes[i]) ? *colourMap.get(featureTypes[i]) : ITKImageUtil::make_rgba32(255,0,255,255);

			typedef typename PartitionForestSelectionT::NodeConstIterator Iter;
			for(Iter jt=selection->nodes_cbegin(), jend=selection->nodes_cend(); jt!=jend; ++jt)
			{
				IPFOverlayTools::draw_node(volumeIPF, *jt, image, sliceBegin, sliceEnd, sliceOrientation, colour, false);
			}
		}

		RGBA32 bc = ITKImageUtil::make_rgba32(255,0,0,255);	// boundary colour
		IPFOverlayTools::draw_boundaries(image, image, bc);

		set_texture(TextureFactory::create_texture(image));
	}
};

}

#endif
