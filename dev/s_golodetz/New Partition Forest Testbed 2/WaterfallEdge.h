/***
 * millipede: WaterfallEdge.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_WATERFALLEDGE
#define H_MILLIPEDE_WATERFALLEDGE

#include <list>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

namespace mp {

//#################### TYPEDEFS ####################
typedef shared_ptr<class WaterfallEdge> WaterfallEdge_Ptr;
typedef shared_ptr<const class WaterfallEdge> WaterfallEdge_CPtr;

/**
The WaterfallEdge class is an abstraction of an edge of the minimum spanning tree on which the
waterfall algorithm operates. Clients of the waterfall algorithm derive from this class and
override the merge_hook() method to specify what should happen when edges in the minimum
spanning tree they provide are merged. Typically, this will involve some merge operation on
the client data structure, but the waterfall algorithm does not (and should not) need to know
about this. The algorithm is only concerned with deciding which edges should be merged.
*/
class WaterfallEdge
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::list<WaterfallEdge_Ptr> m_children;
	WaterfallEdge *m_parent;
	int m_weight;

	//#################### CONSTRUCTORS ####################
public:
	/**
	Constructs a WaterfallEdge with the specified edge weight.

	@param weight	The weight on the edge
	*/
	explicit WaterfallEdge(int weight);

	//#################### DESTRUCTOR ####################
public:
	virtual ~WaterfallEdge();

	//#################### PRIVATE ABSTRACT METHODS ####################
private:
	/**
	A callback function which allows clients to write their own code to be called when an edge
	has been merged.
	*/
	virtual void merge_hook() = 0;

	//#################### PUBLIC METHODS ####################
public:
	/**
	Adds a child edge to this edge.

	@param child	The child to add
	*/
	void add_child(WaterfallEdge_Ptr child);

	/**
	Returns the children of this edge.

	@return	As described
	*/
	std::list<WaterfallEdge_Ptr>& children();

	/**
	Merges the edge, and informs the client that it has merged.

	@param edgeIt	An iterator referring to the edge in its parent's child list
	*/
	void merge(const std::list<WaterfallEdge_Ptr>::iterator& edgeIt);

	/**
	Returns the weight on this edge.

	@return	As described
	*/
	int weight() const;
};

}

#endif
