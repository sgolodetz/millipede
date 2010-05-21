/***
 * millipede: CTImageUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTIMAGEUTIL
#define H_MILLIPEDE_CTIMAGEUTIL

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <itkImage.h>

#include "CTPixelProperties.h"
#include "CTRegionProperties.h"
#include "ImageLeafLayer.h"
#include "ImageBranchLayer.h"

namespace mp {

//#################### TYPEDEFS ####################
typedef ImageLeafLayer<CTPixelProperties, CTRegionProperties> CTImageLeafLayer;
typedef ImageBranchLayer<CTRegionProperties> CTImageBranchLayer;

namespace CTImageUtil {

boost::shared_ptr<CTImageLeafLayer> make_leaf_layer(const itk::Image<int,2>::Pointer& hounsfieldImage, const itk::Image<unsigned char,2>::Pointer& windowedImage);
boost::shared_ptr<CTImageLeafLayer> make_leaf_layer(const itk::Image<int,3>::Pointer& hounsfieldImage, const itk::Image<unsigned char,3>::Pointer& windowedImage);
// TODO: make_mosaic_image

}

}

#endif
