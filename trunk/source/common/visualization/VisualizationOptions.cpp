/***
 * millipede: VisualizationOptions.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "VisualizationOptions.h"

namespace mp {

//#################### CONSTRUCTORS ####################
VisualizationOptions::VisualizationOptions(bool laplacianSmoothingEnabled_, int laplacianSmoothingIterations_, double laplacianSmoothingLambda_,
										   bool meshDecimationEnabled_, int meshDecimationReductionTarget_)
:	laplacianSmoothingEnabled(laplacianSmoothingEnabled_),
	laplacianSmoothingIterations(laplacianSmoothingIterations_),
	laplacianSmoothingLambda(laplacianSmoothingLambda_),
	meshDecimationEnabled(meshDecimationEnabled_),
	meshDecimationReductionTarget(meshDecimationReductionTarget_)
{}

}
