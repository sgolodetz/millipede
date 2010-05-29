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
	typedef boost::shared_ptr<const IPFG> IPFG_CPtr;
	typedef itk::Image<unsigned char,3> MosaicImage;

	//#################### PRIVATE VARIABLES ####################
private:
	IPFG_CPtr m_ipfGrid;
	int m_layerIndex;
	MosaicImage::Pointer& m_mosaicImage;
	bool m_withBoundaries;

	//#################### CONSTRUCTORS ####################
public:
	MosaicImageCreator(const IPFG_CPtr& ipfGrid, int layerIndex, bool withBoundaries, MosaicImage::Pointer& mosaicImage)
	:	m_ipfGrid(ipfGrid), m_layerIndex(layerIndex), m_mosaicImage(mosaicImage), m_withBoundaries(withBoundaries)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void execute()
	{
		set_status("Creating mosaic image...");
		if(m_withBoundaries)	execute_boundaries();
		else					execute_no_boundaries();
		set_finished();
	}

	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_boundaries()
	{
		// NYI
		throw 23;
	}

	void execute_no_boundaries()
	{
		itk::Size<3> size = m_ipfGrid->volume_size();
		m_mosaicImage = ITKImageUtil::make_image<unsigned char>(size);

		MosaicImage::IndexType index;
		for(index[2]=0; index[2]<size[2]; ++index[2])
			for(index[1]=0; index[1]<size[1]; ++index[1])
				for(index[0]=0; index[0]<size[0]; ++index[0])
				{
					IPF_CPtr ipf = m_ipfGrid->forest_of(index[0], index[1], index[2]);
					int n = m_ipfGrid->leaf_index_of(index[0], index[1], index[2]);

					unsigned char mosaicValue;
					if(m_layerIndex > 0)
					{
						PFNodeID ancestor = ipf->ancestor_of(PFNodeID(0, n), m_layerIndex);
						mosaicValue = static_cast<unsigned char>(ipf->branch_properties(ancestor).mean_grey_value());
					}
					else mosaicValue = ipf->leaf_properties(n).grey_value();

					m_mosaicImage->SetPixel(index, mosaicValue);
				}
	}
};

}

#endif
