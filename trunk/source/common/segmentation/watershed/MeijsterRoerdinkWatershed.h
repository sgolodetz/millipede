/***
 * millipede: MeijsterRoerdinkWatershed.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MEIJSTERROERDINKWATERSHED
#define H_MILLIPEDE_MEIJSTERROERDINKWATERSHED

#include <iostream>
#include <limits>
#include <queue>
#include <set>
#include <vector>

#include <boost/shared_ptr.hpp>

#include <itkConstantBoundaryCondition.h>
#include <itkImage.h>
#include <itkImageRegionIterator.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkShapedNeighborhoodIterator.h>

#include <common/adts/DisjointSetForest.h>
#include <common/exceptions/Exception.h>

namespace mp {

/**
@brief	The Meijster/Roerdink watershed algorithm is a specific implementation of the watershed transform for images.

It is described in the paper entitled 'A Disjoint Set Algorithm for the Watershed Transform'.
*/

template <typename InputPixelType, unsigned int Dimension>
class MeijsterRoerdinkWatershed
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<InputPixelType, Dimension> InputImage;
	typedef typename InputImage::Pointer InputImagePointer;

	typedef typename InputImage::IndexType Index;
	typedef itk::Image<Index, Dimension> ArrowImage;
	typedef typename ArrowImage::Pointer ArrowImagePointer;

	typedef std::set<int> Group;

	typedef itk::Image<int, Dimension> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;

	typedef long LowerCompletePixelType;
	typedef itk::Image<LowerCompletePixelType, Dimension> LowerCompleteImage;
	typedef typename LowerCompleteImage::Pointer LowerCompleteImagePointer;

public:
	typedef typename ArrowImage::ConstPointer ArrowImageCPointer;
	typedef boost::shared_ptr<std::vector<Group> > Groups_Ptr;
	typedef typename InputImage::ConstPointer InputImageCPointer;
	typedef typename LabelImage::ConstPointer LabelImageCPointer;
	typedef typename LowerCompleteImage::ConstPointer LowerCompleteImageCPointer;
	typedef itk::Offset<Dimension> NeighbourOffset;
	typedef std::vector<NeighbourOffset> NeighbourOffsets;
	typedef itk::Size<Dimension> NeighbourRadius;

	//#################### PRIVATE VARIABLES ####################
private:
	ArrowImagePointer m_arrows;
	InputImagePointer m_input;
	int m_labelCount;
	LabelImagePointer m_labels;
	LowerCompleteImagePointer m_lowerComplete;
	NeighbourOffsets m_offsets;
	NeighbourRadius m_radius;

	//#################### CONSTRUCTORS ####################
public:
	explicit MeijsterRoerdinkWatershed(const InputImagePointer& input, const NeighbourRadius& radius,
									   const NeighbourOffsets& offsets)
	:	m_input(input), m_offsets(offsets), m_radius(radius)
	{
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
	ArrowImageCPointer arrows() const					{ return ArrowImageCPointer(m_arrows); }

	Groups_Ptr calculate_groups() const
	{
		// NYI
		throw 23;
	}

	InputImageCPointer input() const					{ return InputImageCPointer(m_input); }
	int label_count() const								{ return m_labelCount; }
	LabelImageCPointer labels() const					{ return LabelImageCPointer(m_labels); }
	LowerCompleteImageCPointer lower_complete() const	{ return LowerCompleteImageCPointer(m_lowerComplete); }

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
