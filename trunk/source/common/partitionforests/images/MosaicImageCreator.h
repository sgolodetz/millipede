/***
 * millipede: MosaicImageCreator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MOSAICIMAGECREATOR
#define H_MILLIPEDE_MOSAICIMAGECREATOR

#include <itkConstantBoundaryCondition.h>
#include <itkShapedNeighborhoodIterator.h>

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
		typedef itk::Image<PFNodeID,3> AncestorImage;
		typedef itk::Image<int,3> ForestImage;

		itk::Size<3> volumeSize = m_ipfGrid->volume_size();

		// Create an image of the forest indices of each pixel, and another of their ancestors in the specified layer of their respective forests.
		AncestorImage::Pointer ancestorImage = ITKImageUtil::make_image<PFNodeID>(volumeSize);
		ForestImage::Pointer forestImage = ITKImageUtil::make_image<int>(volumeSize);

		// Note: An index has signed values, whereas a size has unsigned ones. Doing this avoids signed/unsigned mismatch warnings.
		itk::Index<3> size = ITKImageUtil::make_index_from_size(volumeSize);

		itk::Index<3> index;
		for(index[2]=0; index[2]<size[2]; ++index[2])
			for(index[1]=0; index[1]<size[1]; ++index[1])
				for(index[0]=0; index[0]<size[0]; ++index[0])
				{
					int forestIndex = m_ipfGrid->forest_index_of(index[0], index[1], index[2]);
					forestImage->SetPixel(index, forestIndex);

					int n = m_ipfGrid->leaf_index_of(index[0], index[1], index[2]);
					ancestorImage->SetPixel(index, m_ipfGrid->forest(forestIndex)->ancestor_of(PFNodeID(0, n), m_layerIndex));
				}

		// Set up iterators to traverse the forest and ancestor images, whilst allowing us to access the neighbours of each pixel.
		typedef itk::ConstShapedNeighborhoodIterator<AncestorImage> AncestorNIT;
		typedef itk::ConstShapedNeighborhoodIterator<ForestImage> ForestNIT;
		itk::Size<3> radius = {{1,1,1}};
		AncestorNIT ait(radius, ancestorImage, ancestorImage->GetLargestPossibleRegion());
		ForestNIT fit(radius, forestImage, forestImage->GetLargestPossibleRegion());
		std::vector<itk::Offset<3> > offsets = ITKImageUtil::make_6_connected_offsets();
		for(std::vector<itk::Offset<3> >::const_iterator kt=offsets.begin(), kend=offsets.end(); kt!=kend; ++kt)
		{
			ait.ActivateOffset(*kt);
			fit.ActivateOffset(*kt);
		}

		// Set up a boundary condition that makes forest pixels beyond the boundary equal to -1.
		itk::ConstantBoundaryCondition<ForestImage> condition;
		condition.SetConstant(-1);
		fit.OverrideBoundaryCondition(&condition);

		// Create the mosaic image by traversing the forest and ancestor images. We mark boundaries where appropriate,
		// and obtain the non-boundary mosaic values from the properties of the ancestor nodes.
		m_mosaicImage = ITKImageUtil::make_image<unsigned char>(volumeSize);

		for(ait.GoToBegin(), fit.GoToBegin(); !fit.IsAtEnd(); ++ait, ++fit)
		{
			bool regionBoundary = false;
			AncestorNIT::ConstIterator ajt = ait.Begin();
			ForestNIT::ConstIterator fjt = fit.Begin(), fjend = fit.End();
			for(; fjt!=fjend; ++ajt, ++fjt)
			{
				// If one of the pixel's neighbours is in the same forest and has a different ancestor, the pixel is a boundary.
				if(fjt.Get() == fit.GetCenterPixel() && ajt.Get() != ait.GetCenterPixel())
				{
					regionBoundary = true;
					break;
				}
			}

			itk::Index<3> pixelIndex = ait.GetIndex();
			unsigned char mosaicValue;
			if(regionBoundary)
			{
				mosaicValue = std::numeric_limits<unsigned char>::max();
			}
			else
			{
				IPF_CPtr ipf = m_ipfGrid->forest(fit.GetCenterPixel());
				if(m_layerIndex > 0)	mosaicValue = static_cast<unsigned char>(ipf->branch_properties(ait.GetCenterPixel()).mean_grey_value());
				else					mosaicValue = ipf->leaf_properties(ait.GetCenterPixel().index()).grey_value();
			}
			m_mosaicImage->SetPixel(pixelIndex, mosaicValue);
		}
	}

	void execute_no_boundaries()
	{
		itk::Size<3> volumeSize = m_ipfGrid->volume_size();
		m_mosaicImage = ITKImageUtil::make_image<unsigned char>(volumeSize);

		// Note: An index has signed values, whereas a size has unsigned ones. Doing this avoids signed/unsigned mismatch warnings.
		itk::Index<3> size = ITKImageUtil::make_index_from_size(volumeSize);

		itk::Index<3> index;
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
