/***
 * millipede: ITKImageUtil.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "ITKImageUtil.h"

namespace mp {

namespace ITKImageUtil {

//#################### FUNCTIONS ####################
std::vector<itk::Offset<2> > make_4_connected_offsets()
{
	std::vector<itk::Offset<2> > offsets(4);
	offsets[0][0] = 0;		offsets[0][1] = -1;		// above
	offsets[1][0] = -1;		offsets[1][1] = 0;		// left
	offsets[2][0] = 1;		offsets[2][1] = 0;		// right
	offsets[3][0] = 0;		offsets[3][1] = 1;		// below
	return offsets;
}

std::vector<itk::Offset<3> > make_4_connected_offsets(SliceOrientation ori)
{
	std::vector<itk::Offset<3> > offsets(4);
	switch(ori)
	{
		case ORIENT_XY:
			offsets[0][0] = 0;	offsets[0][1] = -1;	offsets[0][2] = 0;
			offsets[1][0] = -1;	offsets[1][1] = 0;	offsets[1][2] = 0;
			offsets[2][0] = 1;	offsets[2][1] = 0;	offsets[2][2] = 0;
			offsets[3][0] = 0;	offsets[3][1] = 1;	offsets[3][2] = 0;
			break;
		case ORIENT_XZ:
			offsets[0][0] = 0;	offsets[0][1] = 0;	offsets[0][2] = -1;
			offsets[1][0] = -1;	offsets[1][1] = 0;	offsets[1][2] = 0;
			offsets[2][0] = 1;	offsets[2][1] = 0;	offsets[2][2] = 0;
			offsets[3][0] = 0;	offsets[3][1] = 0;	offsets[3][2] = 1;
			break;
		case ORIENT_YZ:
			offsets[0][0] = 0;	offsets[0][1] = 0;	offsets[0][2] = -1;
			offsets[1][0] = 0;	offsets[1][1] = -1;	offsets[1][2] = 0;
			offsets[2][0] = 0;	offsets[2][1] = 1;	offsets[2][2] = 0;
			offsets[3][0] = 0;	offsets[3][1] = 0;	offsets[3][2] = 1;
			break;
	}
	return offsets;
}

std::vector<itk::Offset<3> > make_6_connected_offsets()
{
	std::vector<itk::Offset<3> > offsets(6);
	offsets[0][0] = 0;	offsets[0][1] = 0;	offsets[0][2] = -1;
	offsets[1][0] = 0;	offsets[1][1] = -1;	offsets[1][2] = 0;
	offsets[2][0] = -1;	offsets[2][1] = 0;	offsets[2][2] = 0;
	offsets[3][0] = 1;	offsets[3][1] = 0;	offsets[3][2] = 0;
	offsets[4][0] = 0;	offsets[4][1] = 1;	offsets[4][2] = 0;
	offsets[5][0] = 0;	offsets[5][1] = 0;	offsets[5][2] = 1;
	return offsets;
}

}

}
