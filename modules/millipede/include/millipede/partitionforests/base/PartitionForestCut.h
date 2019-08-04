/***
 * millipede: PartitionForestCut.h
 * Copyright Stuart Golodetz, 2018. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFORESTCUT
#define H_MILLIPEDE_PARTITIONFORESTCUT

#include <set>

#include <boost/shared_ptr.hpp>

#include "PFNodeID.h"

namespace mp {

/**
@brief	An instance of a class deriving from this one represents a "cut" across a partition forest.
*/
class PartitionForestCut
{
	//#################### DESTRUCTOR ####################
public:
	/**
	@brief	Destroys the cut.
	*/
	virtual ~PartitionForestCut();

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	/**
	@brief	Gets whether or not the cut contains the specified node.

	@param[in]	node	The node.
	@return	true, if the cut contains the specified node, or false otherwise.
	*/
	virtual bool contains(const PFNodeID& node) const = 0;

	/**
	@brief	Gets the index of the layer containing the shallowest node(s) on the cut.

	@return	The index of the layer containing the shallowest node(s) on the cut.
	*/
	virtual int max_layer() const = 0;

	/**
	@brief	Gets the index of the layer containing the deepest node(s) on the cut.

	@return	The index of the layer containing the deepest node(s) on the cut.
	*/
	virtual int min_layer() const = 0;
};

/**
@brief	An instance of this class represents a horizontal cut across a partition forest.
*/
class HorizontalCut : public PartitionForestCut
{
	//#################### PRIVATE VARIABLES ####################
private:
	/// The layer of the cut.
	int m_layer;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Constructs a horizontal cut at the specified layer.

	@param[in]	layer	The layer of the cut.
	*/
	explicit HorizontalCut(int layer);

	//#################### PUBLIC METHODS ####################
public:
	/** Override */
	virtual bool contains(const PFNodeID& node) const;

	/** Override */
	virtual int max_layer() const;

	/** Override */
	virtual int min_layer() const;
};

/**
@brief	An instance of this class represents a non-horizontal cut across a partition forest.
*/
class NonHorizontalCut : public PartitionForestCut
{
	//#################### PRIVATE VARIABLES ####################
private:
	/// The nodes on the cut.
	std::set<PFNodeID> m_cutNodes;

	/// The index of the layer containing the shallowest node(s) on the cut.
	int m_maxLayer;

	/// The index of the layer containing the deepest node(s) on the cut.
	int m_minLayer;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief Constructs a non-horizontal cut.

	@param[in]	cutNodes	The nodes on the cut.
	*/
	explicit NonHorizontalCut(const std::set<PFNodeID>& cutNodes);

	//#################### PUBLIC METHODS ####################
public:
	/** Override */
	virtual bool contains(const PFNodeID& node) const;

	/** Override */
	virtual int max_layer() const;

	/** Override */
	virtual int min_layer() const;
};

//#################### TYPEDEFS ####################

typedef boost::shared_ptr<const PartitionForestCut> PartitionForestCut_CPtr;

}

#endif
