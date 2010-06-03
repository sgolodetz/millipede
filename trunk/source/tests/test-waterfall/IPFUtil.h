/***
 * millipede: IPFUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFUTIL
#define H_MILLIPEDE_IPFUTIL

#include <common/partitionforests/base/PartitionForest.h>
#include <common/partitionforests/images/CTImageBranchLayer.h>
#include <common/partitionforests/images/CTImageLeafLayer.h>

namespace mp {

namespace IPFUtil {

itk::Image<unsigned char,2>::Pointer make_mosaic_image(const boost::shared_ptr<const PartitionForest<CTImageLeafLayer,CTImageBranchLayer> >& ipf, int layerIndex, int width, int height);
itk::Image<unsigned char,2>::Pointer make_mosaic_image_with_boundaries(const boost::shared_ptr<const PartitionForest<CTImageLeafLayer,CTImageBranchLayer> >& ipf, int layerIndex, int width, int height);

}

}

#endif
