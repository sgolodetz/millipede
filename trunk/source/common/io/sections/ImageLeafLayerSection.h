/***
 * millipede: ImageLeafLayerSection.h
 * Added by Varduhi Yeghiazaryan, 2013.
 ***/

#ifndef H_MILLIPEDE_IMAGELEAFLAYERSECTION
#define H_MILLIPEDE_IMAGELEAFLAYERSECTION

#include <iosfwd>

#include <boost/shared_ptr.hpp>

#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/DICOMPixelProperties.h>

namespace mp {

struct ImageLeafLayerSection
{
	//#################### TYPEDEFS ####################
	typedef DICOMImageLeafLayer ImageLeafLayer;
	typedef ImageLeafLayer::NodeProperties NodeProperties;
	typedef boost::shared_ptr<ImageLeafLayer> ImageLeafLayer_Ptr;

	//#################### LOADING METHODS ####################
	static ImageLeafLayer_Ptr load(std::istream& is);

	//#################### SAVING METHODS ####################
	static void save(std::ostream& os, const ImageLeafLayer_Ptr& imageLeafLayer);
};

}

#endif
