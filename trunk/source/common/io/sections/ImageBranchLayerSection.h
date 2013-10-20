/***
 * millipede: ImageBranchLayerSection.h
 * Added by Varduhi Yeghiazaryan, 2013.
 ***/

#ifndef H_MILLIPEDE_IMAGEBRANCHLAYERSECTION
#define H_MILLIPEDE_IMAGEBRANCHLAYERSECTION

#include <iosfwd>

#include <boost/shared_ptr.hpp>

#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMRegionProperties.h>

namespace mp {

struct ImageBranchLayerSection
{
	//#################### TYPEDEFS ####################
	typedef DICOMImageBranchLayer ImageBranchLayer;
	typedef ImageBranchLayer::NodeProperties NodeProperties;
	typedef boost::shared_ptr<ImageBranchLayer> ImageBranchLayer_Ptr;

	//#################### LOADING METHODS ####################
	static ImageBranchLayer_Ptr load(std::istream& is);

	//#################### SAVING METHODS ####################
	static void save(std::ostream& os, const ImageBranchLayer_Ptr& imageLeafLayer);
};

}

#endif
