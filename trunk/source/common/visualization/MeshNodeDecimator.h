/***
 * millipede: MeshNodeDecimator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHNODEDECIMATOR
#define H_MILLIPEDE_MESHNODEDECIMATOR

namespace mp {

template <typename Label>
class MeshNodeDecimator
{
	//#################### DESTRUCTOR ####################
public:
	virtual ~MeshNodeDecimator() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void calculate_details() = 0;
	virtual double metric() const = 0;
	virtual bool valid() const = 0;
};

}

#endif
