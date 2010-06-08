/***
 * millipede: PartitionModel.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONMODEL
#define H_MILLIPEDE_PARTITIONMODEL

#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <common/partitionforests/base/PartitionForestSelection.h>
#include <common/partitionforests/images/CTImageBranchLayer.h>
#include <common/partitionforests/images/CTImageLeafLayer.h>
#include <common/partitionforests/images/VolumeIPF.h>
#include <common/slices/SliceLocation.h>
#include <common/slices/SliceOrientation.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class DICOMVolume> DICOMVolume_Ptr;
typedef shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;
typedef shared_ptr<class SliceTextureSet> SliceTextureSet_Ptr;
typedef shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;

class PartitionModel
{
	//#################### NESTED CLASSES ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void model_changed() = 0;
	};

	//#################### TYPEDEFS ####################
public:
	typedef VolumeIPF<CTImageLeafLayer,CTImageBranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	typedef PartitionForestSelection<CTImageLeafLayer,CTImageBranchLayer> IPFSelection;
	typedef boost::shared_ptr<IPFSelection> IPFSelection_Ptr;
	typedef boost::shared_ptr<const IPFSelection> IPFSelection_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	SliceTextureSet_Ptr m_dicomTextureSet;
	DICOMVolume_Ptr m_dicomVolume;
	std::vector<SliceTextureSet_Ptr> m_partitionTextureSets;
	IPFSelection_Ptr m_selection;
	SliceLocation m_sliceLocation;			// slice location in terms of the volume only (not based on actual slice numbers)
	SliceOrientation m_sliceOrientation;
	VolumeIPF_Ptr m_volumeIPF;

	std::vector<Listener*> m_listeners;

	//#################### CONSTRUCTORS ####################
public:
	PartitionModel(const DICOMVolume_Ptr& volume, const SliceLocation& loc, SliceOrientation ori);

	//#################### PUBLIC METHODS ####################
public:
	void add_listener(Listener *listener);
	SliceTextureSet_CPtr dicom_texture_set() const;
	DICOMVolume_CPtr dicom_volume() const;
	SliceTextureSet_CPtr partition_texture_set(int layer) const;
	void set_dicom_texture_set(const SliceTextureSet_Ptr& dicomTextureSet);
	void set_partition_texture_sets(const std::vector<SliceTextureSet_Ptr>& partitionTextureSets);
	void set_slice_location(const SliceLocation& loc);
	void set_slice_orientation(SliceOrientation ori);
	void set_volume_ipf(const VolumeIPF_Ptr& volumeIPF);
	const SliceLocation& slice_location() const;
	SliceOrientation slice_orientation() const;
	const VolumeIPF_Ptr& volume_ipf();
	VolumeIPF_CPtr volume_ipf() const;

	//#################### PRIVATE METHODS ####################
private:
	void alert_listeners();
};

//#################### TYPEDEFS ####################
typedef shared_ptr<PartitionModel> PartitionModel_Ptr;
typedef shared_ptr<const PartitionModel> PartitionModel_CPtr;

}

#endif
