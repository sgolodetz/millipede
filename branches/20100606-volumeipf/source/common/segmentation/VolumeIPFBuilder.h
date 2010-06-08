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
	typedef typename LowestLayersBuilder::IPF_Ptr IPF_Ptr;
	typedef typename IPF::LeafLayer LeafLayer;
	typedef typename IPF::BranchLayer BranchLayer;
	typedef boost::shared_ptr<LeafLayer> LeafLayer_Ptr;
	typedef boost::shared_ptr<BranchLayer> BranchLayer_Ptr;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef typename LowestLayersBuilder::SegmentationOptions SegmentationOptions;

	//#################### NESTED CLASSES ####################
private:
	struct ExtractSubvolumeJob : SimpleJob
	{
		VolumeIPFBuilder *base;
		int subvolumeIndex;

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
			base->m_subvolume->reset(new DICOMVolume(extractor->GetOutput()));

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

		explicit CombineLeafLayersJob(VolumeIPFBuilder *base_)
		:	base(base_)
		{}

		void execute()
		{
			set_status("Combining leaf layers...");

			int subvolumeCount = static_cast<int>(base->m_leafLayers.size());
			itk::Size<3> subvolumeSize = base->m_segmentationOptions.subvolumeSize, volumeSize = base->m_volume->size();
			std::vector<CTPixelProperties> nodeProperties(volumeSize[0] * volumeSize[1] * volumeSize[2]);

			for(int i=0; i<subvolumeCount; ++i)
			{
				SubvolumeToVolumeIndexMapper indexMapper(i, subvolumeSize, volumeSize);

				for(typename LeafLayer::LeafNodeConstIterator jt=base->m_leafLayers[i]->leaf_nodes_cbegin(), jend=base->m_leafLayers[i]->leaf_nodes_cend();
					jt!=jend; ++jt)
				{
					// Calculate the index of the leaf node in the combined leaf layer.
					int leafIndex = indexMapper(jt.index());

					// Copy the node's properties across to the correct place in the combined node properties.
					nodeProperties[leafIndex] = jt->properties();
				}
			}

			base->m_combinedLeafLayer.reset(new LeafLayer(nodeProperties, volumeSize[0], volumeSize[1], volumeSize[2]));

			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	struct CombineLowestBranchLayersJob : SimpleJob
	{
		VolumeIPFBuilder *base;

		explicit CombineLowestBranchLayersJob(VolumeIPFBuilder *base_)
		:	base(base_)
		{}

		void execute()
		{
			set_status("Combining lowest branch layers...");

			std::vector<std::set<int> > groups;

			int subvolumeCount = static_cast<int>(base->m_leafLayers.size());
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
			}

			base->m_combinedLowestBranchLayer = VolumeIPFT::make_lowest_branch_layer(base->m_combinedLeafLayer, groups);

			set_finished();
		}

		int length() const
		{
			return 1;
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
				if(is_aborted()) return;
				increment_progress();
			}

			//~~~~~~~
			// STEP 2
			//~~~~~~~

			set_status("Running waterfall...");

			VolumeIPF_Ptr volumeIPF = base->m_volumeIPF;
			itk::Size<3> subvolumeSize = base->m_segmentationOptions.subvolumeSize, volumeSize = base->m_volume->size();

			// Note: We must maintain explicit handles to these listeners while the waterfall passes are running (otherwise it's assumed they've been destroyed).
			typedef WaterfallPass<int>::Listener WaterfallPassListener;
			std::vector<boost::shared_ptr<WaterfallPassListener> > listeners(subvolumeCount);
			std::vector<NichollsWaterfallPass<int> > waterfallPasses(subvolumeCount);
			for(int i=0; i<subvolumeCount; ++i)
			{
				SubvolumeToVolumeIndexMapper indexMapper(i, subvolumeSize, volumeSize);
				listeners[i] = make_forest_building_waterfall_pass_listener(volumeIPF, indexMapper);
				waterfallPasses[i].add_listener(listeners[i]);
			}

			while(volumeIPF->highest_layer() < base->m_segmentationOptions.waterfallLayerLimit)
			{
				volumeIPF->clone_layer(volumeIPF->highest_layer());
				if(is_aborted()) return;

				for(int i=0; i<subvolumeCount; ++i)
				{
					if(msts[i]->node_count() != 1) waterfallPasses[i].run(*msts[i]);
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
	boost::shared_ptr<DICOMVolume_CPtr> m_subvolume;
	DICOMVolume_CPtr m_volume;
	VolumeIPF_Ptr& m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	VolumeIPFBuilder(const DICOMVolume_CPtr& volume, const SegmentationOptions& segmentationOptions, VolumeIPF_Ptr& volumeIPF)
	:	m_segmentationOptions(segmentationOptions), m_subvolume(new DICOMVolume_CPtr), m_volume(volume), m_volumeIPF(volumeIPF)
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
			add_subjob(new ExtractSubvolumeJob(this, i));
			add_subjob(new LowestLayersBuilder(m_subvolume, m_segmentationOptions, m_leafLayers[i], m_lowestBranchLayers[i]));
		}

		add_subjob(new CombineLeafLayersJob(this));
		add_subjob(new CombineLowestBranchLayersJob(this));
		add_subjob(new CreateForestJob(this));
		add_subjob(new WaterfallJob(this));
	}
};

}

#endif
