/***
 * millipede: MosaicImageCreator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MOSAICIMAGECREATOR
#define H_MILLIPEDE_MOSAICIMAGECREATOR

#include <itkImage.h>

#include <common/jobs/SimpleJob.h>
#include <common/partitionforests/images/IPFGrid.h>

namespace mp {

template <typename IPF>
class MosaicImageCreator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef IPFGrid<IPF> IPFG;
	typedef boost::shared_ptr<const IPFG> IPFG_CPtr;
	typedef itk::Image<unsigned char,3> MosaicImage;

	//#################### PRIVATE VARIABLES ####################
private:
	IPFG_CPtr m_ipfGrid;
	int m_layerIndex;
	bool m_withBoundaries;
	MosaicImage::Pointer& m_mosaicImage;

	//#################### CONSTRUCTORS ####################
public:
	MosaicImageCreator(const IPFG_CPtr& ipfGrid, int layerIndex, MosaicImage::Pointer& mosaicImage, bool withBoundaries = true)
	:	m_ipfGrid(ipfGrid), m_layerIndex(layerIndex), m_mosaicImage(mosaicImage), m_withBoundaries(withBoundaries)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void execute()
	{
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
		// NYI
		throw 23;
	}
};

}

#endif
