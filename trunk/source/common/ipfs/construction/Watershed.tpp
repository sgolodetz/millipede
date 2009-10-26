/***
 * millipede: Watershed.tpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#define Watershed_HEADER	template <typename InputPixelType, unsigned int Dimension>
#define Watershed_THIS		Watershed<InputPixelType, Dimension>

#include <limits>

#include "itkConstantBoundaryCondition.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkNeighborhoodIterator.h"

#include <common/adts/DisjointSetForest.h>
#include <common/exceptions/Exception.h>

namespace mp {

//#################### CONSTRUCTORS ####################
Watershed_HEADER
Watershed_THIS::Watershed(const InputImagePointer& function)
:	m_function(function)
{
	// Initialise the radius for the neighbourhood iterators.
	m_radius.Fill(1);

	// Create images with the same dimensions as the input to store the arrows, labels and lower-complete function.
	create_same_size_image(function, m_arrows);
	create_same_size_image(function, m_labels);
	create_same_size_image(function, m_lcFunction);

	// Run the actual algorithm.
	build_lower_complete_function();
	construct_arrows();
	resolve_all();
}

//#################### PUBLIC METHODS ####################
Watershed_HEADER
typename Watershed_THIS::ArrowImageCPointer Watershed_THIS::arrows() const
{
	return ArrowImageCPointer(m_arrows);
}

Watershed_HEADER
typename Watershed_THIS::InputImageCPointer Watershed_THIS::function() const
{
	return InputImageCPointer(m_function);
}

Watershed_HEADER
int Watershed_THIS::label_count() const
{
	return m_labelCount;
}

Watershed_HEADER
typename Watershed_THIS::LabelImageCPointer Watershed_THIS::labels() const
{
	return LabelImageCPointer(m_labels);
}

Watershed_HEADER
typename Watershed_THIS::LowerCompleteImageCPointer Watershed_THIS::lower_complete() const
{
	return LowerCompleteImageCPointer(m_lcFunction);
}

//#################### PRIVATE METHODS ####################
Watershed_HEADER
void Watershed_THIS::build_lower_complete_function()
{
	std::queue<Index> voxelQueue;

	// Initialise the queue with voxels that have a lower neighbour.
	initialise_voxel_queue(voxelQueue);

	// Compute the function described in the paper as f*.
	int dist = compute_f_star(voxelQueue);

	// Compute f_LC.
	compute_f_lc(dist);
}

Watershed_HEADER
void Watershed_THIS::compute_f_lc(int dist)
{
	itk::ImageRegionConstIterator<InputImage> func(m_function, m_function->GetLargestPossibleRegion());
	itk::ImageRegionIterator<LowerCompleteImage> lcFunc(m_lcFunction, m_function->GetLargestPossibleRegion());
	for(func.GoToBegin(), lcFunc.GoToBegin(); !func.IsAtEnd(); ++func, ++lcFunc)
	{
		if(lcFunc.Get() != 0)
		{
			lcFunc.Set(dist * func.Get() + lcFunc.Get() - 1);
		}
	}
}

Watershed_HEADER
int Watershed_THIS::compute_f_star(std::queue<Index>& voxelQueue)
{
	itk::NeighborhoodIterator<InputImage> it(m_radius, m_function, m_function->GetLargestPossibleRegion());

	// We use this boundary condition because the max pixel value will never be equal to
	// the value of the centre pixel (see below).
	itk::ConstantBoundaryCondition<InputImage> condition;
	condition.SetConstant(std::numeric_limits<InputPixelType>::max());
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
			m_lcFunction->SetPixel(p, dist);

			it.SetLocation(p);

			for(unsigned long j=0, size=it.Size(); j<size; ++j)
			{
				if(j == size/2) continue;	// ignore the centre pixel
				Index neighbour = it.GetIndex(j);
				InputPixelType neighbourValue = it.GetPixel(j);
				if(neighbourValue == it.GetCenterPixel() && m_lcFunction->GetPixel(neighbour) == 0)
				{
#if 0
					std::cout << "it: " << it.GetIndex() << "; ";
					std::cout << "Neighbour value: " << neighbourValue << "; Centre pixel: " << it.GetCenterPixel() << "; ";
					std::cout << "Neighbour LC: " << m_lcFunction->GetPixel(neighbour) << '\n';
					std::cout << "Adding " << neighbour << " to queue\n";
#endif
					voxelQueue.push(neighbour);
					m_lcFunction->SetPixel(neighbour, -1);		// to prevent it being queued twice
				}
			}
		}
	}

	return dist;
}

Watershed_HEADER
void Watershed_THIS::construct_arrows()
{
	m_labelCount = 0;

	// Add all the minimum points to a disjoint set forest.
	DisjointSetForest<Index> minima;

	{
	itk::ImageRegionConstIteratorWithIndex<LowerCompleteImage> it(m_lcFunction, m_lcFunction->GetLargestPossibleRegion());
	for(it.GoToBegin(); !it.IsAtEnd(); ++it)
	{
		if(it.Get() == 0)
		{
			m_labels->SetPixel(it.GetIndex(), m_labelCount);
			minima.add_node(m_labelCount, it.GetIndex());
			++m_labelCount;
		}
	}}

	
	{
	itk::NeighborhoodIterator<LowerCompleteImage> it(m_radius, m_lcFunction, m_lcFunction->GetLargestPossibleRegion());

	// We use this boundary condition because the max pixel value will never be equal to
	// zero, or less than the value of the centre pixel (see below).
	itk::ConstantBoundaryCondition<LowerCompleteImage> condition;
	condition.SetConstant(std::numeric_limits<LowerCompletePixelType>::max());
	it.OverrideBoundaryCondition(&condition);

	for(it.GoToBegin(); !it.IsAtEnd(); ++it)
	{
		if(it.GetCenterPixel() == 0)
		{
			// Union any neighbouring minimum points into the same regional minimum.
			for(unsigned long j=0, size=it.Size(); j<size; ++j)
			{
				if(j == size/2) continue;	// ignore the centre pixel
				if(it.GetPixel(j) == 0)
				{
					minima.union_nodes(m_labels->GetPixel(it.GetIndex()), m_labels->GetPixel(it.GetIndex(j)));
				}
			}
		}
		else
		{
			// Find a lowest neighbour and make this point's arrow point to it.
			Index lowestNeighbour;
			LowerCompletePixelType lowestNeighbourValue = std::numeric_limits<LowerCompletePixelType>::max();
			for(unsigned long j=0, size=it.Size(); j<size; ++j)
			{
				if(j == size/2) continue;	// ignore the centre pixel

				LowerCompletePixelType neighbourValue = it.GetPixel(j);
				if(neighbourValue < lowestNeighbourValue)
				{
					lowestNeighbour = it.GetIndex(j);
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

	// Assign new labels to the canonical points of the regional minima and
	// make the arrows of the non-canonical points point to them.
	{
	m_labelCount = 0;
	itk::ImageRegionConstIteratorWithIndex<LowerCompleteImage> it(m_lcFunction, m_lcFunction->GetLargestPossibleRegion());
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

Watershed_HEADER
template <typename InImagePointer, typename OutImagePointer>
void Watershed_THIS::create_same_size_image(InImagePointer source, OutImagePointer& dest)
{
	dest = OutImagePointer::ObjectType::New();
	dest->SetRegions(source->GetLargestPossibleRegion());
	dest->Allocate();
}

Watershed_HEADER
void Watershed_THIS::initialise_voxel_queue(std::queue<Index>& voxelQueue)
{
	itk::NeighborhoodIterator<InputImage> it(m_radius, m_function, m_function->GetLargestPossibleRegion());

	// We use this boundary condition because the max pixel value will never be less than
	// the value of the centre pixel (see below).
	itk::ConstantBoundaryCondition<InputImage> condition;
	condition.SetConstant(std::numeric_limits<InputPixelType>::max());
	it.OverrideBoundaryCondition(&condition);

	for(it.GoToBegin(); !it.IsAtEnd(); ++it)
	{
		m_lcFunction->SetPixel(it.GetIndex(), 0);
		for(unsigned long j=0, size=it.Size(); j<size; ++j)
		{
			if(j == size/2) continue;	// ignore the centre pixel
			if(it.GetPixel(j) < it.GetCenterPixel())
			{
				voxelQueue.push(it.GetIndex());
				m_lcFunction->SetPixel(it.GetIndex(), -1);	// to prevent it being queued twice
				break;
			}
		}
	}
}

Watershed_HEADER
void Watershed_THIS::resolve_all()
{
	itk::ImageRegionConstIteratorWithIndex<InputImage> it(m_function, m_function->GetLargestPossibleRegion());
	for(it.GoToBegin(); !it.IsAtEnd(); ++it)
	{
		resolve_voxel(it.GetIndex());
	}
}

Watershed_HEADER
typename Watershed_THIS::Index Watershed_THIS::resolve_voxel(const Index& v)
{
	Index& parent = m_arrows->GetPixel(v);
	if(parent != v) parent = resolve_voxel(parent);
	m_labels->GetPixel(v) = m_labels->GetPixel(parent);
	return parent;
}

}

#undef Watershed_HEADER
#undef Watershed_THIS
