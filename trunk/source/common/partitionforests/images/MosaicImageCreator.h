/***
 * millipede: MosaicImageCreator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MOSAICIMAGECREATOR
#define H_MILLIPEDE_MOSAICIMAGECREATOR

#include <itkImage.h>

#include <common/io/util/OSSWrapper.h>
#include <common/jobs/SimpleJob.h>
#include <common/partitionforests/images/IPFGrid.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

template <typename IPF>
class MosaicImageCreator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef boost::shared_ptr<const IPF> IPF_CPtr;
	typedef IPFGrid<IPF> IPFG;
	typedef boost::shared_ptr<IPFG> IPFG_Ptr;
	typedef itk::Image<unsigned char,3> MosaicImage;

	//#################### PRIVATE VARIABLES ####################
private:
	boost::shared_ptr<IPFG_Ptr> m_ipfGrid;
	bool m_withBoundaries;
	std::vector<MosaicImage::Pointer>& m_mosaicImages;

	//#################### CONSTRUCTORS ####################
public:
	MosaicImageCreator(const boost::shared_ptr<IPFG_Ptr>& ipfGrid, std::vector<MosaicImage::Pointer>& mosaicImages, bool withBoundaries)
	:	m_ipfGrid(ipfGrid), m_mosaicImages(mosaicImages), m_withBoundaries(withBoundaries)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void execute()
	{
		set_status("Creating mosaic images...");
		int highestLayer = (*m_ipfGrid)->highest_layer();
		if(m_withBoundaries)
		{
			for(int i=1; i<=highestLayer; ++i) execute_boundaries(i);
		}
		else
		{
			for(int i=1; i<=highestLayer; ++i) execute_no_boundaries(i);
		}
		set_finished();
	}

	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_boundaries(int layerIndex)
	{
		// NYI
		throw 23;
	}

	void execute_no_boundaries(int layerIndex)
	{
		itk::Size<3> size = (*m_ipfGrid)->volume_size();
		m_mosaicImages.push_back(ITKImageUtil::make_image<unsigned char>(size));
		MosaicImage::Pointer mosaicImage = m_mosaicImages.back();

		MosaicImage::IndexType index;
		for(index[2]=0; index[2]<size[2]; ++index[2])
			for(index[1]=0; index[1]<size[1]; ++index[1])
				for(index[0]=0; index[0]<size[0]; ++index[0])
				{
					IPF_CPtr ipf = (*m_ipfGrid)->forest_of(index[0], index[1], index[2]);
					int n = (*m_ipfGrid)->leaf_index_of(index[0], index[1], index[2]);

					unsigned char mosaicValue;
					if(layerIndex > 0)
					{
						PFNodeID ancestor = ipf->ancestor_of(PFNodeID(0, n), layerIndex);
						mosaicValue = static_cast<unsigned char>(ipf->branch_properties(ancestor).mean_grey_value());
					}
					else mosaicValue = ipf->leaf_properties(n).grey_value();

					mosaicImage->SetPixel(index, mosaicValue);
				}
	}
};

}

#endif
