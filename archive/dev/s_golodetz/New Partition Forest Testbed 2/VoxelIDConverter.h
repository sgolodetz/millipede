/***
 * millipede: VoxelIDConverter.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOXELIDCONVERTER
#define H_MILLIPEDE_VOXELIDCONVERTER

#include "Voxel.h"

namespace mp {

class VoxelIDConverter
{
public:
	typedef Voxel Actual;

private:
	int m_xSize, m_ySize, m_zSize;

public:
	VoxelIDConverter(int xSize, int ySize, int zSize)
	:	m_xSize(xSize), m_ySize(ySize), m_zSize(zSize)
	{}

public:
	int from_actual(const Voxel& rhs) const
	{
		// NYI
		throw 23;
	}

	Voxel to_actual(int rhs) const
	{
		// NYI
		throw 23;
	}
};

}

#endif
