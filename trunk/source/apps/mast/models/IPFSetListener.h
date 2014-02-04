/***
 * millipede: PartitionView.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFSETLISTENER
#define H_MILLIPEDE_IPFSETLISTENER

//#include "mast/gui/components/partitionview/PartitionView.h"

namespace mp {

//class PartitionView;	
	
class IPFSetListener
{
	
		
	
	public:
		
	virtual void generated(unsigned n) {};
	virtual void swapped(unsigned n) {};
	
	explicit IPFSetListener()
	{}
	
	~IPFSetListener();
};

}

#endif