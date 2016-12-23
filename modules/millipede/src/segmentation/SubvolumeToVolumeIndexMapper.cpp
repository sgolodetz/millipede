/***
 * millipede: SubvolumeToVolumeIndexMapper.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SubvolumeToVolumeIndexMapper.h"

#include <itkIndex.h>

#include <millipede/util/GridUtil.h>

namespace mp {

//#################### CONSTRUCTORS ####################
SubvolumeToVolumeIndexMapper::SubvolumeToVolumeIndexMapper(int subvolumeIndex, const itk::Size<3>& subvolumeSize, const itk::Size<3>& volumeSize)
:	m_subvolumeSize(subvolumeSize), m_volumeSize(volumeSize)
{
	// Step 1:	Calculate the size of the grid.
	itk::Size<3> gridSize;
	for(int i=0; i<3; ++i) gridSize[i] = volumeSize[i] / subvolumeSize[i];

	// Step 2:	Calculate the grid coordinates of the subvolume.
	itk::Size<3> gridPos = {{	GridUtil::x_of(subvolumeIndex, gridSize[0]),
								GridUtil::y_of(subvolumeIndex, gridSize[0], gridSize[1]),
								GridUtil::z_of(subvolumeIndex, gridSize[0] * gridSize[1])	}};

	// Step 3:	Calculate the starting position of the subvolume.
	for(int i=0; i<3; ++i)
	{
		m_subvolumeStart[i] = gridPos[i] * m_subvolumeSize[i];
	}
}

//#################### PUBLIC OPERATORS ####################
int SubvolumeToVolumeIndexMapper::operator()(int nodeIndex) const
{
	// Step 1:	Calculate the position relative to the start of the subvolume.
	itk::Index<3> subvolumePosition = {{	GridUtil::x_of(nodeIndex, m_subvolumeSize[0]),
											GridUtil::y_of(nodeIndex, m_subvolumeSize[0], m_subvolumeSize[1]),
											GridUtil::z_of(nodeIndex, m_subvolumeSize[0] * m_subvolumeSize[1])	}};

	// Step 2:	Offset by the position of the start of the subvolume.
	itk::Index<3> volumePosition = subvolumePosition + m_subvolumeStart;

	// Step 3:	Convert to a volume index.
	return (volumePosition[2] * m_volumeSize[1] + volumePosition[1]) * m_volumeSize[0] + volumePosition[0];
}

}
