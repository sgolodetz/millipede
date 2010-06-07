/***
 * millipede: VolumeIPFBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMEIPFBUILDER
#define H_MILLIPEDE_VOLUMEIPFBUILDER

#include <itkRegionOfInterestImageFilter.h>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/io/util/OSSWrapper.h>
#include <common/jobs/CompositeJob.h>
#include <common/partitionforests/images/VolumeIPF.h>
#include <common/util/GridUtil.h>

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

	struct LeafLayersCombinerJob : SimpleJob
	{
		VolumeIPFBuilder *base;

		explicit LeafLayersCombinerJob(VolumeIPFBuilder *base_)
		:	base(base_)
		{}

		void execute()
		{
			set_status("Combining leaf layers...");

			// TODO

			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	struct LowestBranchLayersCombinerJob : SimpleJob
	{
		VolumeIPFBuilder *base;

		explicit LowestBranchLayersCombinerJob(VolumeIPFBuilder *base_)
		:	base(base_)
		{}

		void execute()
		{
			set_status("Combining lowest branch layers...");

			// TODO

			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	struct ForestCreatorJob : SimpleJob
	{
		VolumeIPFBuilder *base;

		explicit ForestCreatorJob(VolumeIPFBuilder *base_)
		:	base(base_)
		{}

		void execute()
		{
			set_status("Creating initial partition forest...");

			// TODO

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
		int subvolumeIndex;

		WaterfallJob(VolumeIPFBuilder *base_, int subvolumeIndex_)
		:	base(base_), subvolumeIndex(subvolumeIndex_)
		{}

		void execute()
		{
			set_status(OSSWrapper() << "Running waterfall " << subvolumeIndex << "...");

			// TODO

			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
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

		add_subjob(new LeafLayersCombinerJob(this));
		add_subjob(new LowestBranchLayersCombinerJob(this));
		add_subjob(new ForestCreatorJob(this));

		for(int i=0; i<subvolumeCount; ++i)
		{
			add_subjob(new WaterfallJob(this, i));
		}
	}
};

}

#endif
