/***
 * millipede: MosaicImageCreator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MOSAICIMAGECREATOR
#define H_MILLIPEDE_MOSAICIMAGECREATOR

#include <itkShapedNeighborhoodIterator.h>
#include <itkZeroFluxNeumannBoundaryCondition.h>

#include <millipede/io/util/OSSWrapper.h>
#include <millipede/jobs/DataHook.h>
#include <millipede/jobs/SimpleJob.h>
#include <millipede/partitionforests/images/VolumeIPF.h>
#include <millipede/slices/SliceOrientation.h>
#include <millipede/util/ITKImageUtil.h>

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class MosaicImageCreator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<unsigned char,3> MosaicImage;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_layerIndex;
	DataHook<MosaicImage::Pointer> m_mosaicImageHook;
	SliceOrientation m_sliceOrientation;
	VolumeIPF_CPtr m_volumeIPF;
	bool m_withBoundaries;

	//#################### CONSTRUCTORS ####################
public:
	MosaicImageCreator(const VolumeIPF_CPtr& volumeIPF, int layerIndex, SliceOrientation sliceOrientation, bool withBoundaries)
	:	m_layerIndex(layerIndex), m_sliceOrientation(sliceOrientation), m_volumeIPF(volumeIPF), m_withBoundaries(withBoundaries)
	{}

	//#################### PUBLIC METHODS ####################
public:
	const DataHook<MosaicImage::Pointer>& get_mosaic_image_hook() const
	{
		return m_mosaicImageHook;
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

		// Create an image containing the ancestors of the leaves corresponding to the pixels in the specified layer of the forest.
		itk::Size<3> volumeSize = m_volumeIPF->volume_size();
		AncestorImage::Pointer ancestorImage = ITKImageUtil::make_image<PFNodeID>(volumeSize);

		// Note: An index has signed values, whereas a size has unsigned ones. Doing this avoids signed/unsigned mismatch warnings.
		itk::Index<3> size = ITKImageUtil::make_index_from_size(volumeSize);

		int pixelCount = size[0] * size[1] * size[2];

#ifdef WITH_OPENMP
		#pragma omp parallel for
#endif
		for(int i = 0; i < pixelCount; ++i)
		{
			// i = z * size[0] * size[1] + y * size[0] + x
			int z = i / (size[0] * size[1]);
			int tmp = i - z * size[0] * size[1];
			int y = tmp / size[0];
			int x = tmp - y * size[0];
			itk::Index<3> p = {{x, y, z}};
			ancestorImage->SetPixel(p, m_volumeIPF->node_of(m_layerIndex, p));
		}

		// Set up an iterator to traverse the ancestor image, whilst allowing us to access the neighbours of each pixel.
		// The neighbours in this case are used only to determine whether or not a pixel is on a boundary, and as such are
		// dependent on the orientation of the slices being viewed.
		typedef itk::ConstShapedNeighborhoodIterator<AncestorImage> NIT;
		itk::Size<3> radius = {{1,1,1}};
		NIT it(radius, ancestorImage, ancestorImage->GetLargestPossibleRegion());
		std::vector<itk::Offset<3> > offsets = ITKImageUtil::make_4_connected_offsets(m_sliceOrientation);
		for(std::vector<itk::Offset<3> >::const_iterator kt=offsets.begin(), kend=offsets.end(); kt!=kend; ++kt)
		{
			it.ActivateOffset(*kt);
		}

		// Set up a boundary condition that makes ancestor pixels beyond the image boundary equal to those on them.
		itk::ZeroFluxNeumannBoundaryCondition<AncestorImage> condition;
		it.OverrideBoundaryCondition(&condition);

		// Create the mosaic image by traversing the ancestor image. We mark boundaries where appropriate,
		// and obtain the non-boundary mosaic values from the properties of the ancestor nodes.
		MosaicImage::Pointer mosaicImage = ITKImageUtil::make_image<unsigned char>(volumeSize);

		for(it.GoToBegin(); !it.IsAtEnd(); ++it)
		{
			bool regionBoundary = false;
			for(NIT::ConstIterator jt=it.Begin(), jend=it.End(); jt!=jend; ++jt)
			{
				// If one of the pixel's neighbours has a different ancestor, the pixel is a boundary.
				if(jt.Get() != it.GetCenterPixel())
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
				if(m_layerIndex > 0)	mosaicValue = static_cast<unsigned char>(m_volumeIPF->branch_properties(it.GetCenterPixel()).mean_grey_value());
				else					mosaicValue = m_volumeIPF->leaf_properties(it.GetCenterPixel().index()).grey_value();
			}
			mosaicImage->SetPixel(it.GetIndex(), mosaicValue);
		}

		m_mosaicImageHook.set(mosaicImage);
	}

	void execute_impl()
	{
		set_status("Creating mosaic image...");
		if(m_withBoundaries)	execute_boundaries();
		else					execute_no_boundaries();
	}

	void execute_no_boundaries()
	{
		itk::Size<3> volumeSize = m_volumeIPF->volume_size();
		MosaicImage::Pointer mosaicImage = ITKImageUtil::make_image<unsigned char>(volumeSize);

		// Note: An index has signed values, whereas a size has unsigned ones. Doing this avoids signed/unsigned mismatch warnings.
		itk::Index<3> size = ITKImageUtil::make_index_from_size(volumeSize);

		int pixelCount = size[0] * size[1] * size[2];

#ifdef WITH_OPENMP
		#pragma omp parallel for
#endif
		for(int i = 0; i < pixelCount; ++i)
		{
			// i = z * size[0] * size[1] + y * size[0] + x
			int z = i / (size[0] * size[1]);
			int tmp = i - z * size[0] * size[1];
			int y = tmp / size[0];
			int x = tmp - y * size[0];
			itk::Index<3> index = {{x, y, z}};

			int n = m_volumeIPF->leaf_of_position(index);

			unsigned char mosaicValue;
			if(m_layerIndex > 0)
			{
				PFNodeID ancestor = m_volumeIPF->ancestor_of(PFNodeID(0,n), m_layerIndex);
				mosaicValue = static_cast<unsigned char>(m_volumeIPF->branch_properties(ancestor).mean_grey_value());
			}
			else mosaicValue = m_volumeIPF->leaf_properties(n).grey_value();

			mosaicImage->SetPixel(index, mosaicValue);
		}

		m_mosaicImageHook.set(mosaicImage);
	}
};

}

#endif
