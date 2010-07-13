/***
 * millipede: VisualizationOptions.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VISUALIZATIONOPTIONS
#define H_MILLIPEDE_VISUALIZATIONOPTIONS

namespace mp {

struct VisualizationOptions
{
	//#################### PUBLIC VARIABLES ####################
	int laplacianSmoothingIterations;
	double laplacianSmoothingLambda;
	int meshDecimationReductionTarget;

	//#################### CONSTRUCTORS ####################
	VisualizationOptions(int laplacianSmoothingIterations_, double laplacianSmoothingLambda_, int meshDecimationReductionTarget_);
};

}

#endif
