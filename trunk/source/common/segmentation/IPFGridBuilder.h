/***
 * millipede: IPFGridBuilder.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFGRIDBUILDER
#define H_MILLIPEDE_IPFGRIDBUILDER

#include <itkRegionOfInterestImageFilter.h>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/jobs/CompositeJob.h>
#include <common/partitionforests/images/IPFGrid.h>
#include <common/util/GridUtil.h>

namespace mp {

template <typename IPFBuilder>
class IPFGridBuilder : public CompositeJob
{
	//#################### TYPEDEFS ####################
public:
	typedef typename IPFBuilder::IPF IPF;
	typedef typename IPFBuilder::IPF_Ptr IPF_Ptr;
	typedef IPFGrid<IPF> IPFG;
	typedef boost::shared_ptr<IPFG> IPFG_Ptr;
	typedef typename IPFBuilder::SegmentationOptions SegmentationOptions;

	//#################### NESTED CLASSES ####################
private:
	struct ExtractSubvolumeJob : SimpleJob
	{
		IPFGridBuilder *base;
		int forestIndex;

		ExtractSubvolumeJob(IPFGridBuilder *base_, int forestIndex_)
		:	base(base_), forestIndex(forestIndex_)
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
			index[0] = GridUtil::x_of(forestIndex, gridSize[0]) * subvolumeSize[0];
			index[1] = GridUtil::y_of(forestIndex, gridSize[0], gridSize[1]) * subvolumeSize[1];
			index[2] = GridUtil::z_of(forestIndex, gridSize[0] * gridSize[1]) * subvolumeSize[2];
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

	struct GridCreatorJob : SimpleJob
	{
		IPFGridBuilder *base;

		GridCreatorJob(IPFGridBuilder *base_)
		:	base(base_)
		{}

		void execute()
		{
			set_status("Creating forest grid...");
			base->m_ipfGrid->reset(new IPFG(base->m_forests, base->m_segmentationOptions.subvolumeSize, base->m_volume->size()));
			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<IPF_Ptr> m_forests;
	boost::shared_ptr<IPFG_Ptr> m_ipfGrid;
	itk::Size<3> m_gridSize;
	SegmentationOptions m_segmentationOptions;
	boost::shared_ptr<DICOMVolume_CPtr> m_subvolume;
	DICOMVolume_CPtr m_volume;

	//#################### CONSTRUCTORS ####################
public:
	IPFGridBuilder(const DICOMVolume_CPtr& volume, const SegmentationOptions& segmentationOptions, const boost::shared_ptr<IPFG_Ptr>& ipfGrid)
	:	m_ipfGrid(ipfGrid), m_segmentationOptions(segmentationOptions), m_subvolume(new DICOMVolume_CPtr), m_volume(volume)
	{
		itk::Size<3> volumeSize = volume->size();
		itk::Size<3> subvolumeSize = segmentationOptions.subvolumeSize;

		int forestCount = 1;
		for(int i=0; i<3; ++i)
		{
			m_gridSize[i] = volumeSize[i] / subvolumeSize[i];
			forestCount *= m_gridSize[i];
		}

		m_forests.resize(forestCount);

		for(int i=0; i<forestCount; ++i)
		{
			add_subjob(new ExtractSubvolumeJob(this, i));
			add_subjob(new IPFBuilder(m_subvolume, m_segmentationOptions, m_forests[i]));
		}

		add_subjob(new GridCreatorJob(this));
	}
};

}

#endif
