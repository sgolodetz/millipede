/***
 * millipede: CTImageUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTIMAGEUTIL
#define H_MILLIPEDE_CTIMAGEUTIL

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include "ImageLeafLayer.h"

namespace mp {

namespace CTImageUtil {

boost::shared_ptr<ImageLeafLayer> make_leaf_layer(const itk::Image<int,2>::Pointer& hounsfieldImage, const itk::Image<unsigned char,2>::Pointer& windowedImage);
boost::shared_ptr<ImageLeafLayer> make_leaf_layer(const itk::Image<int,3>::Pointer& hounsfieldImage, const itk::Image<unsigned char,3>::Pointer& windowedImage);
// TODO: make_mosaic_image

}

}

#endif
