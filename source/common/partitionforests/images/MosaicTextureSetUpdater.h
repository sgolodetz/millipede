/***
 * millipede: MosaicTextureSetUpdater.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MOSAICTEXTURESETUPDATER
#define H_MILLIPEDE_MOSAICTEXTURESETUPDATER

#include <algorithm>

#include <common/jobs/SimpleJob.h>
#include <common/util/ITKImageUtil.h>
#include "VolumeIPF.h"

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class MosaicTextureSetUpdater : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_layerIndex;
	Greyscale8SliceTextureSet_Ptr m_mosaicTextureSet;
	std::set<int> m_nodes;
	SliceOrientation m_sliceOrientation;
	VolumeIPF_CPtr m_volumeIPF;
	bool m_withBoundaries;

	//#################### CONSTRUCTORS ####################
public:
	MosaicTextureSetUpdater(const Greyscale8SliceTextureSet_Ptr& mosaicTextureSet, int layerIndex, const std::set<int>& nodes,
							const VolumeIPF_CPtr& volumeIPF, SliceOrientation sliceOrientation, bool withBoundaries)
	:	m_layerIndex(layerIndex),
		m_mosaicTextureSet(mosaicTextureSet),
		m_nodes(nodes),
		m_sliceOrientation(sliceOrientation),
		m_volumeIPF(volumeIPF),
		m_withBoundaries(withBoundaries)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl()
	{
		set_status("Updating mosaic texture set...");

		// Step 1:	Build maps in both directions between nodes and pixels.
		std::map<int,PFNodeID> nodeLookup;
		std::map<PFNodeID,std::vector<int> > pixelLookup;
		for(std::set<int>::const_iterator it=m_nodes.begin(), iend=m_nodes.end(); it!=iend; ++it)
		{
			PFNodeID node(m_layerIndex, *it);
			std::deque<int> receptiveRegion = m_volumeIPF->receptive_region_of(node);
			for(std::deque<int>::const_iterator jt=receptiveRegion.begin(), jend=receptiveRegion.end(); jt!=jend; ++jt)
			{
				nodeLookup.insert(std::make_pair(*jt, node));
				pixelLookup[node].push_back(*jt);
			}
		}

		// Step 2:	Calculate the mosaic values for each pixel, and update the mosaic texture set accordingly.
		std::vector<itk::Offset<3> > offsets = ITKImageUtil::make_4_connected_offsets(m_sliceOrientation);

		for(std::map<PFNodeID,std::vector<int> >::const_iterator it=pixelLookup.begin(), iend=pixelLookup.end(); it!=iend; ++it)
		{
			const PFNodeID& ancestor = it->first;
			for(std::vector<int>::const_iterator jt=it->second.begin(), jend=it->second.end(); jt!=jend; ++jt)
			{
				itk::Index<3> pos = m_volumeIPF->position_of_leaf(*jt);

				bool regionBoundary = false;
				for(size_t k=0, size=offsets.size(); k<size; ++k)
				{
					int adjLeaf = m_volumeIPF->leaf_of_position(pos + offsets[k]);
					std::map<int,PFNodeID>::const_iterator adjIt = nodeLookup.find(adjLeaf);
					if(adjIt == nodeLookup.end() || adjIt->second != ancestor)	// if either an outer boundary or an inner boundary
					{
						regionBoundary = true;
						break;
					}
				}

				unsigned char mosaicValue;
				if(regionBoundary)
				{
					mosaicValue = std::numeric_limits<unsigned char>::max();
				}
				else
				{
					if(m_layerIndex > 0)	mosaicValue = static_cast<unsigned char>(m_volumeIPF->branch_properties(ancestor).mean_grey_value());
					else					mosaicValue = m_volumeIPF->leaf_properties(ancestor.index()).grey_value();
				}
				m_mosaicTextureSet->set_pixel(m_sliceOrientation, pos, mosaicValue);
			}
		}
	}
};

}

#endif
