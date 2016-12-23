/***
 * millipede: PFNodeID.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PFNODEID
#define H_MILLIPEDE_PFNODEID

#include <iosfwd>

namespace mp {

/**
@brief	A partition forest node ID is a (layer,index) pair that uniquely identifies a node in a partition forest.

The layer component of the pair specifies the layer of the forest in which the node resides,
where 0 is the leaf layer, 1 is the lowest branch layer, etc. For the node ID to be valid,
the layer must be in the range [0,h], where h is the index of the highest layer of the
forest. The index component of the pair simply specifies the node's index within its layer.
*/
class PFNodeID
{
	//#################### PRIVATE VARIABLES ####################
private:
	// Note:	Partition forests are shallow data structures - they will never have > 127 layers (so a larger type would be wasteful).
	//			This makes a practical difference when allocating large numbers of PFNodeIDs (e.g. during mosaic image creation).
	signed char m_layer;
	int m_index;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Constructs the (invalid) node ID (-1,-1).
	*/
	PFNodeID();

	/**
	@brief	Constructs the node ID (layer,index).

	@param[in]	layer	The layer component of the ID
	@param[in]	index	The index component of the ID
	*/
	PFNodeID(int layer, int index);

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Returns the node ID's index component.

	@return	As described
	*/
	int index() const;

	/**
	@brief	Returns the node ID's layer component.

	@return	As described
	*/
	int layer() const;

	/**
	@brief	Tests whether this node ID is equal to another.

	@param[in]	rhs		The other node ID
	@return	true, if the node IDs are equal, or false if they are different
	*/
	bool operator==(const PFNodeID& rhs) const;

	/**
	@brief	Tests whether this node ID is different from another.

	@param[in]	rhs		The other node ID
	@return	true, if the node IDs are different, or false if they are equal
	*/
	bool operator!=(const PFNodeID& rhs) const;

	/**
	@brief	Tests whether this node ID is lexicographically less than another.

	This implicitly defines a strict total order on node IDs.

	@param[in]	rhs		The other node ID
	@return	true, if this node ID is lexicographically less than the other, or false otherwise
	*/
	bool operator<(const PFNodeID& rhs) const;

	/**
	@brief	Returns the (invalid) node ID (-1,-1).

	This is only provided to improve readability in client code, since the default constructor
	constructs the same thing.

	@return	As described
	*/
	static PFNodeID invalid();
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const PFNodeID& rhs);

}

#endif
