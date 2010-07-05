/***
 * millipede: MultilayerNodeScorerOverlay.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MULTILAYERNODESCOREROVERLAY
#define H_MILLIPEDE_MULTILAYERNODESCOREROVERLAY

#include <common/featureid/multilayer/MultilayerNodeScorer.h>
#include <common/util/NumericUtil.h>
#include "IPFOverlayTools.h"
#include "PartitionOverlay.h"

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class MultilayerNodeScorerOverlay : public PartitionOverlay
{
	//#################### TYPEDEFS ####################
private:
	typedef MultilayerNodeScorer<LeafLayer,BranchLayer> MultilayerNodeScorerT;
	typedef boost::shared_ptr<MultilayerNodeScorerT> MultilayerNodeScorer_Ptr;
	typedef typename MultilayerNodeScorerT::Scores Scores;
	typedef boost::shared_ptr<const VolumeIPF<LeafLayer,BranchLayer> > VolumeIPF_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	MultilayerNodeScorer_Ptr m_scorer;
	VolumeIPF_CPtr m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	MultilayerNodeScorerOverlay(const MultilayerNodeScorer_Ptr& scorer, const VolumeIPF_CPtr& volumeIPF,
								const SliceLocation& sliceLocation, SliceOrientation sliceOrientation)
	:	m_scorer(scorer), m_volumeIPF(volumeIPF)
	{
		recreate_texture(sliceLocation, sliceOrientation);
	}

	//#################### PUBLIC METHODS ####################
public:
	void iterate()
	{
		m_scorer->iterate();
	}

	void recreate_texture(const SliceLocation& sliceLocation, SliceOrientation sliceOrientation)
	{
		itk::Index<3> sliceBegin, sliceEnd;
		int width, height;
		IPFOverlayTools::calculate_slice_parameters(m_volumeIPF->volume_size(), sliceLocation, sliceOrientation, sliceBegin, sliceEnd, width, height);

		RGBA32Image::Pointer image = ITKImageUtil::make_image<RGBA32>(width, height);

		const Scores& scores = m_scorer->scores();
		for(typename Scores::const_iterator it=scores.begin(), iend=scores.end(); it!=iend; ++it)
		{
			if(it->first.layer() != sliceLocation.layer) continue;

			int k = NumericUtil::round_to_nearest<int>(255 * it->second);
			if(k < 0) k = 0;
			if(k > 255) k = 255;
			unsigned char c = static_cast<unsigned char>(k);
			RGBA32 colour = ITKImageUtil::make_rgba32(255 - c, c, 0, 50);
			IPFOverlayTools::draw_node(m_volumeIPF, it->first, image, sliceBegin, sliceEnd, sliceOrientation, colour, false);
		}

		set_texture(TextureFactory::create_texture(image));
	}

	void reset()
	{
		m_scorer->reset();
	}
};

}

#endif
