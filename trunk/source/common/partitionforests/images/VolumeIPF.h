/***
 * millipede: VolumeIPF.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMEIPF
#define H_MILLIPEDE_VOLUMEIPF

#include <itkIndex.h>
#include <itkSize.h>

#include <common/partitionforests/base/PartitionForest.h>
#include <common/util/GridUtil.h>

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class VolumeIPF : public PartitionForest<LeafLayer,BranchLayer>
{
	//#################### TYPEDEFS ####################
private:
	typedef typename PartitionForest<LeafLayer,BranchLayer>::LeafLayer_Ptr LeafLayer_Ptr;
	typedef typename PartitionForest<LeafLayer,BranchLayer>::BranchLayer_Ptr BranchLayer_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	itk::Size<3> m_volumeSize;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Constructs an initial volume IPF from the volume size, a leaf layer (and, optionally, a corresponding
			lowest branch layer).

	See the documentation of PartitionForest's constructor for more details about the optional lowest branch layer.

	@param[in]	volumeSize			The size of the volume represented by the IPF
	@param[in]	leafLayer			A shared_ptr (non-null) to the leaf layer
	@param[in]	lowestBranchLayer	An optional shared_ptr (possibly null) to the lowest branch layer
	@pre
		-	leafLayer.get() != NULL
		-	volumeSize is equal to the size of the leaf layer
		-	The layers pointed to by leafLayer and lowestBranchLayer properly correspond to each other
			(in the obvious manner)
	*/
	explicit VolumeIPF(const itk::Size<3>& volumeSize, const LeafLayer_Ptr& leafLayer, const BranchLayer_Ptr& lowestBranchLayer = BranchLayer_Ptr())
	:	PartitionForest<LeafLayer,BranchLayer>(leafLayer, lowestBranchLayer), m_volumeSize(volumeSize)
	{}

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Calculates the index of the leaf node with the specified position in the volume.

	@param[in]	position	A position in the volume
	@return The index of the corresponding leaf node
	*/
	int leaf_of_position(const itk::Index<3>& position) const
	{
		return (position[2] * m_volumeSize[1] + position[1]) * m_volumeSize[0] + position[0];
	}

	/**
	@brief	Calculates the position in the volume of the leaf node with the specified index.

	@param[in]	leafIndex	The index of the leaf
	@return The position of the leaf in the volume
	*/
	itk::Index<3> position_of_leaf(int leafIndex) const
	{
		itk::Index<3> position = {{	GridUtil::x_of(leafIndex, m_volumeSize[0]),
									GridUtil::y_of(leafIndex, m_volumeSize[0], m_volumeSize[1]),
									GridUtil::z_of(leafIndex, m_volumeSize[0] * m_volumeSize[1])	}};
		return position;
	}

	/**
	@brief	Returns the size of the volume represented by the partition forest.

	@return As described
	*/
	const itk::Size<3>& volume_size() const
	{
		return m_volumeSize;
	}
};

}

#endif
