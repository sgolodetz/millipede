/***
 * millipede: VolumeIPFBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMEIPFBUILDER
#define H_MILLIPEDE_VOLUMEIPFBUILDER

#include <itkRegionOfInterestImageFilter.h>

#include <common/adts/RootedMST.h>
#include <common/dicom/volumes/DICOMVolume.h>
#include <common/io/util/OSSWrapper.h>
#include <common/jobs/CompositeJob.h>
#include <common/jobs/DataHook.h>
#include <common/partitionforests/images/VolumeIPF.h>
#include <common/segmentation/waterfall/NichollsWaterfallPass.h>
#include <common/util/GridUtil.h>
#include "ForestBuildingWaterfallPassListener.h"
#include "SubvolumeToVolumeIndexMapper.h"

namespace mp {

template <typename LowestLayersBuilder>
class VolumeIPFBuilder : public CompositeJob
{
	//#################### TYPEDEFS ####################
public:
	typedef typename LowestLayersBuilder::IPF IPF;
	typedef typename IPF::LeafLayer LeafLayer;
	typedef typename IPF::BranchLayer BranchLayer;
	typedef boost::shared_ptr<LeafLayer> LeafLayer_Ptr;
	typedef boost::shared_ptr<BranchLayer> BranchLayer_Ptr;
	typedef typename LeafLayer::NodeProperties LeafProperties;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef typename LowestLayersBuilder::SegmentationOptions SegmentationOptions;

	//#################### NESTED CLASSES ####################
private:
	struct ExtractSubvolumeJob : SimpleJob
	{
		VolumeIPFBuilder *base;
		int subvolumeIndex;
		DataHook<DICOMVolume_CPtr> subvolumeHook;

		ExtractSubvolumeJob(VolumeIPFBuilder *base_, int subvolumeIndex_)
		:	base(base_), subvolumeIndex(subvolumeIndex_)
		{}

		void execute()
		{
			set_status("Extracting subvolume...");

			typedef DICOMVolume::BaseImage Image;
			typedef itk::RegionOfInterestImageFilter<Image,Image> RegionExtractor;
			RegionExtractor::Pointer extractor = RegionExtractor::New();
			extractor->SetInput(base->m_volume->base_image());

			// Set the region to extract.
			const itk::Size<3>& gridSize = base->m_gridSize;
			const itk::Size<3>& subvolumeSize = base->m_segmentationOptions.subvolumeSize;
			Image::RegionType region;
			Image::IndexType index;
			index[0] = GridUtil::x_of(subvolumeIndex, gridSize[0]) * subvolumeSize[0];
			index[1] = GridUtil::y_of(subvolumeIndex, gridSize[0], gridSize[1]) * subvolumeSize[1];
			index[2] = GridUtil::z_of(subvolumeIndex, gridSize[0] * gridSize[1]) * subvolumeSize[2];
			region.SetIndex(index);
			region.SetSize(base->m_segmentationOptions.subvolumeSize);
			extractor->SetRegionOfInterest(region);

			extractor->Update();
			subvolumeHook.set(DICOMVolume_CPtr(new DICOMVolume(extractor->GetOutput(), base->m_volume->modality())));

			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	struct CombineLeafLayersJob : SimpleJob
	{
		VolumeIPFBuilder *base;
		int subvolumeCount;

		explicit CombineLeafLayersJob(VolumeIPFBuilder *base_)
		:	base(base_), subvolumeCount(static_cast<int>(base->m_leafLayers.size()))
		{}

		void execute()
		{
			set_status("Combining leaf layers...");
			itk::Size<3> subvolumeSize = base->m_segmentationOptions.subvolumeSize, volumeSize = base->m_volume->size();
			std::vector<LeafProperties> leafProperties(volumeSize[0] * volumeSize[1] * volumeSize[2]);
			for(int i=0; i<subvolumeCount; ++i)
			{
				SubvolumeToVolumeIndexMapper indexMapper(i, subvolumeSize, volumeSize);

				for(typename LeafLayer::LeafNodeConstIterator jt=base->m_leafLayers[i]->leaf_nodes_cbegin(), jend=base->m_leafLayers[i]->leaf_nodes_cend();
					jt!=jend; ++jt)
				{
					// Calculate the index of the leaf node in the combined leaf layer.
					int leafIndex = indexMapper(jt.index());

					// Copy the leaf's properties across to the correct place in the combined leaf properties.
					leafProperties[leafIndex] = jt->properties();
				}

				// There's no more use for the subvolume's leaf layer, so free up the memory (space is at a premium during forest construction).
				base->m_leafLayers[i].reset();

				if(is_aborted()) return;
				increment_progress();
			}
			base->m_combinedLeafLayer.reset(new LeafLayer(leafProperties, volumeSize[0], volumeSize[1], volumeSize[2]));
			set_finished();
		}

		int length() const
		{
			return subvolumeCount;
		}
	};

	struct CombineLowestBranchLayersJob : SimpleJob
	{
		VolumeIPFBuilder *base;
		int subvolumeCount;

		explicit CombineLowestBranchLayersJob(VolumeIPFBuilder *base_)
		:	base(base_), subvolumeCount(static_cast<int>(base->m_leafLayers.size()))
		{}

		void execute()
		{
			set_status("Combining lowest branch layers...");
			std::vector<std::set<int> > groups;
			itk::Size<3> subvolumeSize = base->m_segmentationOptions.subvolumeSize, volumeSize = base->m_volume->size();
			for(int i=0; i<subvolumeCount; ++i)
			{
				SubvolumeToVolumeIndexMapper indexMapper(i, subvolumeSize, volumeSize);

				for(typename BranchLayer::BranchNodeConstIterator jt=base->m_lowestBranchLayers[i]->branch_nodes_cbegin(),
					jend=base->m_lowestBranchLayers[i]->branch_nodes_cend(); jt!=jend; ++jt)
				{
					const std::set<int>& children = jt->children();
					std::set<int> group;
					for(std::set<int>::const_iterator kt=children.begin(), kend=children.end(); kt!=kend; ++kt)
					{
						group.insert(indexMapper(*kt));
					}
					groups.push_back(group);
				}

				if(is_aborted()) return;
				increment_progress();
			}
			base->m_combinedLowestBranchLayer = VolumeIPFT::make_lowest_branch_layer(base->m_combinedLeafLayer, groups);
			set_finished();
		}

		int length() const
		{
			return subvolumeCount;
		}
	};

	struct CreateForestJob : SimpleJob
	{
		VolumeIPFBuilder *base;

		explicit CreateForestJob(VolumeIPFBuilder *base_)
		:	base(base_)
		{}

		void execute()
		{
			set_status("Creating initial partition forest...");
			base->m_volumeIPF.reset(new VolumeIPFT(base->m_volume->size(), base->m_combinedLeafLayer, base->m_combinedLowestBranchLayer));
			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	struct WaterfallJob : SimpleJob
	{
		VolumeIPFBuilder *base;
		int subvolumeCount;

		explicit WaterfallJob(VolumeIPFBuilder *base_)
		:	base(base_), subvolumeCount(static_cast<int>(base->m_leafLayers.size()))
		{}

		void execute()
		{
			//~~~~~~~
			// STEP 1
			//~~~~~~~

			std::vector<boost::shared_ptr<RootedMST<int> > > msts(subvolumeCount);
			for(int i=0; i<subvolumeCount; ++i)
			{
				set_status(OSSWrapper() << "Creating rooted MST " << i << "...");
				msts[i].reset(new RootedMST<int>(*(base->m_lowestBranchLayers[i])));

				// There's no more use for the subvolume's lowest branch layer, so free up the memory (space is at a premium during forest construction).
				base->m_lowestBranchLayers[i].reset();

				if(is_aborted()) return;
				increment_progress();
			}

			//~~~~~~~
			// STEP 2
			//~~~~~~~

			set_status("Running waterfall...");

			VolumeIPF_Ptr volumeIPF = base->m_volumeIPF;
			itk::Size<3> subvolumeSize = base->m_segmentationOptions.subvolumeSize, volumeSize = base->m_volume->size();

			std::vector<NichollsWaterfallPass<int> > waterfallPasses(subvolumeCount);
			for(int i=0; i<subvolumeCount; ++i)
			{
				SubvolumeToVolumeIndexMapper indexMapper(i, subvolumeSize, volumeSize);
				waterfallPasses[i].add_shared_listener(make_forest_building_waterfall_pass_listener(volumeIPF, indexMapper));
			}

			while(volumeIPF->highest_layer() < base->m_segmentationOptions.waterfallLayerLimit)
			{
				volumeIPF->clone_layer(volumeIPF->highest_layer());
				if(is_aborted()) return;

				for(int i=0; i<subvolumeCount; ++i)
				{
					if(msts[i]->node_count() != 1)
					{
						waterfallPasses[i].run(*msts[i]);
					}
				}
				if(is_aborted()) return;
			}

			set_finished();
		}

		int length() const
		{
			return subvolumeCount + 1;
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	LeafLayer_Ptr m_combinedLeafLayer;
	BranchLayer_Ptr m_combinedLowestBranchLayer;
	itk::Size<3> m_gridSize;
	std::vector<LeafLayer_Ptr> m_leafLayers;
	std::vector<BranchLayer_Ptr> m_lowestBranchLayers;
	SegmentationOptions m_segmentationOptions;
	DICOMVolume_CPtr m_volume;
	VolumeIPF_Ptr& m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	VolumeIPFBuilder(const DICOMVolume_CPtr& volume, const SegmentationOptions& segmentationOptions, VolumeIPF_Ptr& volumeIPF)
	:	m_segmentationOptions(segmentationOptions), m_volume(volume), m_volumeIPF(volumeIPF)
	{
		itk::Size<3> volumeSize = volume->size();
		itk::Size<3> subvolumeSize = segmentationOptions.subvolumeSize;

		int subvolumeCount = 1;
		for(int i=0; i<3; ++i)
		{
			m_gridSize[i] = volumeSize[i] / subvolumeSize[i];
			subvolumeCount *= m_gridSize[i];
		}

		m_leafLayers.resize(subvolumeCount);
		m_lowestBranchLayers.resize(subvolumeCount);

		for(int i=0; i<subvolumeCount; ++i)
		{
			ExtractSubvolumeJob *extractor = new ExtractSubvolumeJob(this, i);
			LowestLayersBuilder *builder = new LowestLayersBuilder(m_segmentationOptions, m_leafLayers[i], m_lowestBranchLayers[i]);
			builder->set_volume_hook(extractor->subvolumeHook);
			add_subjob(extractor);
			add_subjob(builder);
		}

		add_subjob(new CombineLeafLayersJob(this));
		add_subjob(new CombineLowestBranchLayersJob(this));
		add_subjob(new CreateForestJob(this));
		add_subjob(new WaterfallJob(this));
	}
};

}

#endif
