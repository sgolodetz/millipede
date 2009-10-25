/***
 * millipede: Watershed.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_WATERSHED
#define H_MILLIPEDE_WATERSHED

#include <queue>

#include "itkImage.h"

namespace mp {

/**
This class template implements an algorithm based on the one described in the paper
'A Disjoint Set Algorithm for the Watershed Transform'.
*/
template <typename InputPixelType, unsigned int Dimension>
class Watershed
{
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

	//#################### PRIVATE VARIABLES ####################
private:
	ArrowImagePointer m_arrows;
	InputImagePointer m_function;
	int m_labelCount;
	LabelImagePointer m_labels;
	LowerCompleteImagePointer m_lcFunction;

	itk::Size<Dimension> m_radius;

	//#################### CONSTRUCTORS ####################
public:
	explicit Watershed(const InputImagePointer& function);

	//#################### PUBLIC METHODS ####################
public:
	ArrowImageCPointer arrows() const;
	InputImageCPointer function() const;
	int label_count() const;
	LabelImageCPointer labels() const;
	LowerCompleteImageCPointer lower_complete() const;

	//#################### PRIVATE METHODS ####################
private:
	void build_lower_complete_function();
	void compute_f_lc(int dist);
	int compute_f_star(std::queue<Index>& voxelQueue);
	void construct_arrows();
	template <typename InImagePointer, typename OutImagePointer> static void create_same_size_image(InImagePointer source, OutImagePointer& dest);
	void initialise_voxel_queue(std::queue<Index>& voxelQueue);
	void resolve_all();
	Index resolve_voxel(const Index& v);
};

}

#include "Watershed.tpp"

#endif
