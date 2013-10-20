/***
 * millipede: VolumeIPFSection.h
 * Added by Varduhi Yeghiazaryan, 2013.
 ***/

#ifndef H_MILLIPEDE_VOLUMEIPFSECTION
#define H_MILLIPEDE_VOLUMEIPFSECTION

#include <iosfwd>

#include <boost/shared_ptr.hpp>

#include <common/io/sections/ImageBranchLayerSection.h>
#include <common/io/sections/ImageLeafLayerSection.h>

#include <common/partitionforests/images/VolumeIPF.h>

namespace mp {

struct VolumeIPFSection
{
	//#################### TYPEDEFS ####################
	typedef ImageLeafLayerSection::ImageLeafLayer LeafLayer;	
	typedef ImageBranchLayerSection::ImageBranchLayer BranchLayer;
	typedef boost::shared_ptr<LeafLayer> LeafLayer_Ptr;	
	typedef boost::shared_ptr<BranchLayer> BranchLayer_Ptr;
	typedef VolumeIPF<LeafLayer, BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;

	//#################### LOADING METHODS ####################
	static VolumeIPF_Ptr load(std::istream& is);

	//#################### SAVING METHODS ####################
	static void save(std::ostream& os, const VolumeIPF_Ptr& volumeIPF);
};

}

#endif
