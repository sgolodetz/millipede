/***
 * millipede: PFNodeID.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PFNODEID
#define H_MILLIPEDE_PFNODEID

#include <iosfwd>

namespace mp {

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
	PFNodeID();
	PFNodeID(int layer, int index);

	//#################### PUBLIC METHODS ####################
public:
	int index() const;
	int layer() const;

	bool operator==(const PFNodeID& rhs) const;
	bool operator!=(const PFNodeID& rhs) const;
	bool operator<(const PFNodeID& rhs) const;

	static PFNodeID invalid();
};

//#################### GLOBAL OPERATORS ####################
std::ostream& operator<<(std::ostream& os, const PFNodeID& rhs);

}

#endif
