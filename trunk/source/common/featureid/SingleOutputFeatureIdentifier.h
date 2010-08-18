/***
 * millipede: SingleOutputFeatureIdentifier.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SINGLEOUTPUTFEATUREIDENTIFIER
#define H_MILLIPEDE_SINGLEOUTPUTFEATUREIDENTIFIER

#include "ISingleOutputFeatureIdentifier.h"

namespace mp {

class SingleOutputFeatureIdentifier : public ISingleOutputFeatureIdentifier
{
	//#################### PRIVATE VARIABLES ####################
private:
	DICOMVolume_CPtr m_dicomVolume;
	DataHook<VolumeIPFMultiFeatureSelection_Ptr> m_mfsHook;
	VolumeIPF_Ptr m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	SingleOutputFeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& get_mfs_hook() const;
	const VolumeIPFMultiFeatureSelection_Ptr& get_multi_feature_selection() const;

	//#################### PROTECTED METHODS ####################
protected:
	DICOMVolume_CPtr dicom_volume() const;
	void set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook);
	VolumeIPF_CPtr volume_ipf() const;
};

}

#endif
