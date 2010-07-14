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
	bool laplacianSmoothingEnabled;
	int laplacianSmoothingIterations;
	double laplacianSmoothingLambda;
	bool meshDecimationEnabled;
	int meshDecimationReductionTarget;

	//#################### CONSTRUCTORS ####################
	VisualizationOptions(bool laplacianSmoothingEnabled_, int laplacianSmoothingIterations_, double laplacianSmoothingLambda_,
						 bool meshDecimationEnabled_, int meshDecimationReductionTarget_);
};

}

#endif
