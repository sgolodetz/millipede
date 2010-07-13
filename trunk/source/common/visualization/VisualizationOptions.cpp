/***
 * millipede: VisualizationOptions.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "VisualizationOptions.h"

namespace mp {

//#################### CONSTRUCTORS ####################
VisualizationOptions::VisualizationOptions(int laplacianSmoothingIterations_, double laplacianSmoothingLambda_, int meshDecimationReductionTarget_)
:	laplacianSmoothingIterations(laplacianSmoothingIterations_),
	laplacianSmoothingLambda(laplacianSmoothingLambda_),
	meshDecimationReductionTarget(meshDecimationReductionTarget_)
{}

}
