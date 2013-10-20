/***
 * millipede: VolumeIPFFile.h
 * Added by Varduhi Yeghiazaryan, 2013.
 ***/

#ifndef H_MILLIPEDE_VOLUMEIPFFILE
#define H_MILLIPEDE_VOLUMEIPFFILE

#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/VolumeIPF.h>

namespace mp {

struct VolumeIPFFile
{
	//#################### TYPEDEFS ####################	
	typedef VolumeIPF<DICOMImageLeafLayer,DICOMImageBranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;

	//#################### LOADING METHODS ####################
	static VolumeIPF_Ptr load(const std::string& filename);

	//#################### SAVING METHODS ####################
	static void save(const std::string& filename, const VolumeIPF_Ptr& volumeIPF);
};

}

#endif
