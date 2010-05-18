/***
 * millipede: MeijsterRoerdinkWatershed.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MEIJSTERROERDINKWATERSHED
#define H_MILLIPEDE_MEIJSTERROERDINKWATERSHED

#include <cassert>
#include <iostream>
#include <limits>
#include <queue>
#include <set>
#include <vector>

#include <boost/mpl/assert.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/type_traits/is_integral.hpp>

#include <itkConstantBoundaryCondition.h>
#include <itkImage.h>
#include <itkImageRegionIterator.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkShapedNeighborhoodIterator.h>

#include <common/adts/DisjointSetForest.h>
#include <common/exceptions/Exception.h>

namespace mp {

/**
@brief	The Meijster/Roerdink watershed algorithm is one possible image-specific version of the watershed
		transform, which is a general segmentation approach based on the idea of dividing a landscape into
		its catchment basins. The algorithm is described in the paper entitled 'A Disjoint Set Algorithm for
		the Watershed Transform'.

This class provides an ITK-based implementation of it that works for both 2D and 3D images.

@tparam InputPixelType	The pixel type of the input image (must be integral)
@tparam Dimension		The dimensionality of the input image (generally 2 or 3)
*/
template <typename InputPixelType, unsigned int Dimension>
class MeijsterRoerdinkWatershed
{
	//#################### TEMPLATE PARAMETER CONSTRAINTS ####################
	BOOST_MPL_ASSERT_MSG(boost::is_integral<InputPixelType>::value,
						 NON_INTEGRAL_PIXEL_TYPES_ARE_NOT_SUPPORTED,
						 (InputPixelType));

	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<InputPixelType, Dimension> InputImage;
	typedef typename InputImage::Pointer InputImagePointer;

	typedef typename InputImage::IndexType Index;
	typedef itk::Image<Index, Dimension> ArrowImage;
	typedef typename ArrowImage::Pointer ArrowImagePointer;

	typedef itk::Image<int, Dimension> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;

	typedef long LowerCompletePixelType;
	typedef itk::Image<LowerCompletePixelType, Dimension> LowerCompleteImage;
	typedef typename LowerCompleteImage::Pointer LowerCompleteImagePointer;

public:
	typedef typename ArrowImage::ConstPointer ArrowImageCPointer;
	typedef typename InputImage::ConstPointer InputImageCPointer;
	typedef typename LabelImage::ConstPointer LabelImageCPointer;
	typedef typename LowerCompleteImage::ConstPointer LowerCompleteImageCPointer;
	typedef itk::Offset<Dimension> NeighbourOffset;
	typedef std::vector<NeighbourOffset> NeighbourOffsets;

	//#################### PRIVATE VARIABLES ####################
private:
	ArrowImagePointer m_arrows;
	InputImagePointer m_input;
	int m_labelCount;
	LabelImagePointer m_labels;
	LowerCompleteImagePointer m_lowerComplete;
	NeighbourOffsets m_offsets;
	itk::Size<Dimension> m_radius;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Runs the Meijster/Roerdink watershed algorithm on the input image. The various results
			can then be extracted using the public member functions.

	The offsets parameter is used to specify the desired connectivity of the pixels. As an example, we can
	specify that the pixels should be 4-connected (in 2D) as follows:

	@code
WS::NeighbourOffsets offsets(4);
offsets[0][0] = 0;		offsets[0][1] = -1;		// above
offsets[1][0] = -1;		offsets[1][1] = 0;		// left
offsets[2][0] = 1;		offsets[2][1] = 0;		// right
offsets[3][0] = 0;		offsets[3][1] = 1;		// below
	@endcode

	It is worth noting that the order in which the offsets are specified here makes no difference to the
	result of running the algorithm, since they are simply passed on to ITK, which chooses its own order.

	@param[in]	input		An itk::SmartPointer to the input image
	@param[in]	offsets		A vector of itk::Offset used to specify the desired connectivity of the pixels
	@pre
		-	input.IsNotNull()
	@post
		-	arrows().IsNotNull()
		-	labels().IsNotNull()
		-	lower_complete().IsNotNull()
	*/
	explicit MeijsterRoerdinkWatershed(const InputImagePointer& input, const NeighbourOffsets& offsets)
	:	m_input(input), m_offsets(offsets)
	{
		assert(input.IsNotNull());

		m_radius.Fill(1);

		// Create images with the same dimensions as the input to store the arrows, labels and lower-complete function.
		create_same_size_image(input, m_arrows);
		create_same_size_image(input, m_labels);
		create_same_size_image(input, m_lowerComplete);

		// Run the actual algorithm.
		build_lower_complete_function();
		construct_arrows();
		resolve_all();
	}

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Returns the arrows image.

	The arrows image is an image of locations. After the algorithm has been run, each pixel in the arrow image
	contains the location of the canonical point of the catchment basin to which it belongs.

	@return As described above
	*/
	ArrowImageCPointer arrows() const
	{
		return ArrowImageCPointer(m_arrows);
	}

	/**
	@brief	Calculates the pixel groups induced by the watershed labelling.

	@see labels

	@return The pixel groups
	*/
	std::vector<std::set<int> > calculate_groups() const
	{
		std::vector<std::set<int> > groups(m_labelCount);

		typename LabelImage::SizeType size = m_labels->GetLargestPossibleRegion().GetSize();

		itk::ImageRegionConstIteratorWithIndex<LabelImage> it(m_labels, m_labels->GetLargestPossibleRegion());
		for(it.GoToBegin(); !it.IsAtEnd(); ++it)
		{
			int group = it.Get();
			Index location = it.GetIndex();
			int n = 0;

			if(Dimension == 2)
			{
				n = location[1] * size[0] + location[0];
			}
			else if(Dimension == 3)
			{
				n = location[2] * size[0] * size[1] + location[1] * size[0] + location[0];
			}
			else
			{
				for(int j=0; j<Dimension; ++j)
				{
					int term = location[j];
					for(int k=0; k<j; ++k)
					{
						term *= size[k];
					}
					n += term;
				}
			}

			groups[group].insert(n);
		}

		return groups;
	}

	/**
	@brief	Returns the input image.

	@return As described above
	*/
	InputImageCPointer input() const
	{
		return InputImageCPointer(m_input);
	}

	/**
	@brief	Returns the number of labels in the label image.

	@see labels

	@return As described above
	*/
	int label_count() const
	{
		return m_labelCount;
	}

	/**
	@brief	Returns the label image.

	The label image is effectively the main output of the Meijster/Roerdink watershed algorithm. It is an image
	of integers in the range [0, label_count() - 1]. The numbers themselves are of little significance; what is
	important is the implied clustering of the pixels into groups, such that all the pixels labelled 0 are in one
	group, all those labelled 1 are in another, and so on. The groups themselves can be determined (in the obvious
	manner) using the calculate_groups() method.

	@return As described above
	*/
	LabelImageCPointer labels() const
	{
		return LabelImageCPointer(m_labels);
	}

	/**
	@brief	Returns the lower complete image.

	The lower-complete image is a transformation of the original image whose corresponding discrete landscape
	contains no non-minimal plateaux. The transformation works by raising non-minimal plateau pixels based on
	their distance from their plateau's boundary.

	@return As described above
	*/
	LowerCompleteImageCPointer lower_complete() const
	{
		return LowerCompleteImageCPointer(m_lowerComplete);
	}

	//#################### PRIVATE METHODS ####################
private:
	void build_lower_complete_function()
	{
		std::queue<Index> voxelQueue;
		initialise_voxel_queue(voxelQueue);		// initialise the queue with voxels that have a lower neighbour
		int dist = compute_f_star(voxelQueue);	// compute the function described in the paper as f*
		compute_f_lc(dist);						// compute f_LC
	}

	void compute_f_lc(int dist)
	{
		itk::ImageRegionConstIterator<InputImage> it(m_input, m_input->GetLargestPossibleRegion());
		itk::ImageRegionIterator<LowerCompleteImage> lcIt(m_lowerComplete, m_input->GetLargestPossibleRegion());
		for(it.GoToBegin(), lcIt.GoToBegin(); !it.IsAtEnd(); ++it, ++lcIt)
		{
			if(lcIt.Get() != 0)
			{
				lcIt.Set(dist * it.Get() + lcIt.Get() - 1);
			}
		}
	}

	int compute_f_star(std::queue<Index>& voxelQueue)
	{
		typedef itk::ConstShapedNeighborhoodIterator<InputImage> ConstShapedNeighbourhoodIteratorType;
		ConstShapedNeighbourhoodIteratorType it(m_radius, m_input, m_input->GetLargestPossibleRegion());
		for(typename NeighbourOffsets::const_iterator ot=m_offsets.begin(), oend=m_offsets.end(); ot!=oend; ++ot)
		{
			it.ActivateOffset(*ot);
		}

		// Set a boundary condition that causes the minimum pixel value to be returned for
		// pixel locations outside the image. This ensures that a non-existent pixel will
		// never be added to the queue, since the queue was initialised with pixels that
		// had a lower neighbour, and only pixels with values equal to that of a pixel
		// already on the queue will be added below.
		itk::ConstantBoundaryCondition<InputImage> condition;
		condition.SetConstant(std::numeric_limits<InputPixelType>::min());
		it.OverrideBoundaryCondition(&condition);

		Index markerPoint;	// a marker used to indicate when we need to increase the distance value
		markerPoint.Fill(-1);

		int dist = 1;
		voxelQueue.push(markerPoint);

		while(!voxelQueue.empty())
		{
			Index p = voxelQueue.front();
			voxelQueue.pop();

			if(p == markerPoint)
			{
				if(!voxelQueue.empty())
				{
					voxelQueue.push(markerPoint);
					++dist;
				}
			}
			else
			{
#if 0
				std::cout << "Setting " << p << " to " << dist << '\n';
#endif
				m_lowerComplete->SetPixel(p, dist);

				it.SetLocation(p);
				for(typename ConstShapedNeighbourhoodIteratorType::ConstIterator jt=it.Begin(), jend=it.End(); jt!=jend; ++jt)
				{
					Index neighbour = it.GetIndex() + jt.GetNeighborhoodOffset();
					InputPixelType neighbourValue = jt.Get();
					if(neighbourValue == it.GetCenterPixel() && m_lowerComplete->GetPixel(neighbour) == 0)
					{
#if 0
						std::cout << "Adding neighbour: " << neighbour << ", " << neighbourValue << '\n';
#endif
						voxelQueue.push(neighbour);
						m_lowerComplete->SetPixel(neighbour, -1);	// to prevent it being queued twice
					}
				}
			}
		}

		return dist;
	}

	void construct_arrows()
	{
		m_labelCount = 0;
		DisjointSetForest<Index> minima;

		// Step 1:	Add all the minimum points to a disjoint set forest.
		{
		itk::ImageRegionConstIteratorWithIndex<LowerCompleteImage> it(m_lowerComplete, m_lowerComplete->GetLargestPossibleRegion());
		for(it.GoToBegin(); !it.IsAtEnd(); ++it)
		{
			if(it.Get() == 0)
			{
				m_labels->SetPixel(it.GetIndex(), m_labelCount);
				minima.add_node(m_labelCount, it.GetIndex());
				++m_labelCount;
			}
		}}

		// Step 2:	Iterate over the lower-complete image, merging regional minima and
		//			adding arrows to non-minimal points.
		{
		typedef itk::ConstShapedNeighborhoodIterator<LowerCompleteImage> ConstShapedNeighbourhoodIteratorType;
		ConstShapedNeighbourhoodIteratorType it(m_radius, m_lowerComplete, m_lowerComplete->GetLargestPossibleRegion());
		for(typename NeighbourOffsets::const_iterator ot=m_offsets.begin(), oend=m_offsets.end(); ot!=oend; ++ot)
		{
			it.ActivateOffset(*ot);
		}

		// Set a boundary condition that causes the maximum pixel value to be returned for
		// pixel locations outside the image. This is appropriate because the maximum pixel
		// value will never be equal to zero (so there will be no attempts to merge pixels
		// that don't exist into regional minima below) and it will never be less than the
		// value of the pixel at the centre of the iterator (so no arrow will be added that
		// points to a non-existent pixel).
		itk::ConstantBoundaryCondition<LowerCompleteImage> condition;
		condition.SetConstant(std::numeric_limits<LowerCompletePixelType>::max());
		it.OverrideBoundaryCondition(&condition);

		for(it.GoToBegin(); !it.IsAtEnd(); ++it)
		{
			if(it.GetCenterPixel() == 0)
			{
				// Union any neighbouring minimum points into the same regional minimum.
				for(typename ConstShapedNeighbourhoodIteratorType::ConstIterator jt=it.Begin(), jend=it.End(); jt!=jend; ++jt)
				{
					if(jt.Get() == 0)
					{
						Index neighbourIndex = it.GetIndex() + jt.GetNeighborhoodOffset();
						minima.union_nodes(m_labels->GetPixel(it.GetIndex()), m_labels->GetPixel(neighbourIndex));
					}
				}
			}
			else
			{
				// Find a lowest neighbour and make this point's arrow point to it.
				Index lowestNeighbour;
				LowerCompletePixelType lowestNeighbourValue = std::numeric_limits<LowerCompletePixelType>::max();
				for(typename ConstShapedNeighbourhoodIteratorType::ConstIterator jt=it.Begin(), jend=it.End(); jt!=jend; ++jt)
				{
					LowerCompletePixelType neighbourValue = jt.Get();
					if(neighbourValue < lowestNeighbourValue)
					{
						lowestNeighbour = it.GetIndex() + jt.GetNeighborhoodOffset();
						lowestNeighbourValue = neighbourValue;
					}
				}

				if(lowestNeighbourValue < it.GetCenterPixel())
				{
					m_arrows->SetPixel(it.GetIndex(), lowestNeighbour);
				}
				else throw Exception("This should never happen since the function is lower-complete");
			}
		}}

		// Step 3:	Assign new labels to the canonical points of the regional minima and
		//			make the arrows of the non-canonical points point to them.
		{
		m_labelCount = 0;
		itk::ImageRegionConstIteratorWithIndex<LowerCompleteImage> it(m_lowerComplete, m_lowerComplete->GetLargestPossibleRegion());
		for(it.GoToBegin(); !it.IsAtEnd(); ++it)
		{
			if(it.Get() != 0) continue;

			Index index = it.GetIndex();
			int label = m_labels->GetPixel(index);
			int rootLabel = minima.find_set(label);
			if(rootLabel == label)	// if this is a canonical point
			{
				m_arrows->SetPixel(index, index);
				m_labels->SetPixel(index, m_labelCount++);
			}
			else
			{
				m_arrows->SetPixel(index, minima.value_of(rootLabel));
				m_labels->SetPixel(index, -1);	// technically, this is redundant, but it makes the intermediate results more comprehensible
			}
		}}
	}

	template <typename InImagePointer, typename OutImagePointer>
	static void create_same_size_image(InImagePointer source, OutImagePointer& dest)
	{
		dest = OutImagePointer::ObjectType::New();
		dest->SetRegions(source->GetLargestPossibleRegion());
		dest->Allocate();
	}

	/**
	@brief	Initialise the queue with voxels that have a lower neighbour.

	@param[out]	voxelQueue	The queue to initialise
	*/
	void initialise_voxel_queue(std::queue<Index>& voxelQueue)
	{
		typedef itk::ConstShapedNeighborhoodIterator<InputImage> ConstShapedNeighbourhoodIteratorType;
		ConstShapedNeighbourhoodIteratorType it(m_radius, m_input, m_input->GetLargestPossibleRegion());
		for(typename NeighbourOffsets::const_iterator ot=m_offsets.begin(), oend=m_offsets.end(); ot!=oend; ++ot)
		{
			it.ActivateOffset(*ot);
		}

		// Set a boundary condition that causes the maximum pixel value to be returned for
		// pixel locations outside the image. This will ensure that a pixel will never be
		// added to the queue as a result of being "higher than" a non-existent pixel.
		itk::ConstantBoundaryCondition<InputImage> condition;
		condition.SetConstant(std::numeric_limits<InputPixelType>::max());
		it.OverrideBoundaryCondition(&condition);

		for(it.GoToBegin(); !it.IsAtEnd(); ++it)
		{
			m_lowerComplete->SetPixel(it.GetIndex(), 0);
			for(typename ConstShapedNeighbourhoodIteratorType::ConstIterator jt=it.Begin(), jend=it.End(); jt!=jend; ++jt)
			{
				if(jt.Get() < it.GetCenterPixel())
				{
					voxelQueue.push(it.GetIndex());
					m_lowerComplete->SetPixel(it.GetIndex(), -1);	// to prevent it being queued twice
					break;
				}
			}
		}
	}

	void resolve_all()
	{
		itk::ImageRegionConstIteratorWithIndex<InputImage> it(m_input, m_input->GetLargestPossibleRegion());
		for(it.GoToBegin(); !it.IsAtEnd(); ++it)
		{
			resolve_voxel(it.GetIndex());
		}
	}

	Index resolve_voxel(const Index& v)
	{
		Index& parent = m_arrows->GetPixel(v);
		if(parent != v) parent = resolve_voxel(parent);
		m_labels->GetPixel(v) = m_labels->GetPixel(parent);
		return parent;
	}
};

}

#endif
