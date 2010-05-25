/***
 * millipede: PartitionModelListener.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONMODELLISTENER
#define H_MILLIPEDE_PARTITIONMODELLISTENER

namespace mp {

class PartitionModelListener
{
	//#################### DESTRUCTOR ####################
public:
	virtual ~PartitionModelListener() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void model_changed() = 0;
};

}

#endif
