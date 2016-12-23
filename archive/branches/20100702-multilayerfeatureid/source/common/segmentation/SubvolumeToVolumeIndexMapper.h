/***
 * millipede: SubvolumeToVolumeIndexMapper.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SUBVOLUMETOVOLUMEINDEXMAPPER
#define H_MILLIPEDE_SUBVOLUMETOVOLUMEINDEXMAPPER

#include <itkSize.h>

namespace mp {

class SubvolumeToVolumeIndexMapper
{
	//#################### PRIVATE VARIABLES ####################
private:
	itk::Size<3> m_subvolumeStart;
	itk::Size<3> m_subvolumeSize;
	itk::Size<3> m_volumeSize;

	//#################### CONSTRUCTORS ####################
public:
	SubvolumeToVolumeIndexMapper(int subvolumeIndex, const itk::Size<3>& subvolumeSize, const itk::Size<3>& volumeSize);

	//#################### PUBLIC OPERATORS ####################
public:
	int operator()(int nodeIndex) const;
};

}

#endif
