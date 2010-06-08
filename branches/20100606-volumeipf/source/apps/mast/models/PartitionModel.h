/***
 * millipede: PartitionModel.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONMODEL
#define H_MILLIPEDE_PARTITIONMODEL

#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <common/partitionforests/images/CTImageBranchLayer.h>
#include <common/partitionforests/images/CTImageLeafLayer.h>
#include <common/partitionforests/images/IPFGrid.h>
#include <common/partitionforests/images/IPFSelectionGrid.h>
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
	typedef PartitionForest<CTImageLeafLayer,CTImageBranchLayer> IPF;

#if 1
	typedef IPFGrid<IPF> IPFGridT;
	typedef boost::shared_ptr<IPFGridT> IPFGrid_Ptr;
	typedef boost::shared_ptr<const IPFGridT> IPFGrid_CPtr;
#endif

	typedef PartitionForestSelection<CTImageLeafLayer,CTImageBranchLayer> IPFSelection;
	typedef boost::shared_ptr<IPFSelection> IPFSelection_Ptr;

#if 1
	typedef IPFSelectionGrid<IPFSelection> IPFSelectionGridT;
	typedef boost::shared_ptr<IPFSelectionGridT> IPFSelectionGrid_Ptr;
	typedef boost::shared_ptr<const IPFSelectionGridT> IPFSelectionGrid_CPtr;
#endif

	typedef VolumeIPF<CTImageLeafLayer,CTImageBranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	SliceTextureSet_Ptr m_dicomTextureSet;
	DICOMVolume_Ptr m_dicomVolume;
#if 1
	IPFGrid_Ptr m_ipfGrid;
#endif
	std::vector<SliceTextureSet_Ptr> m_partitionTextureSets;
	IPFSelection_Ptr m_selection;
#if 1
	IPFSelectionGrid_Ptr m_selectionGrid;
#endif
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
#if 1
	const IPFGrid_Ptr& ipf_grid();
	IPFGrid_CPtr ipf_grid() const;
#endif
	SliceTextureSet_CPtr partition_texture_set(int layer) const;
#if 1
	const IPFSelectionGrid_Ptr& selection_grid();
	IPFSelectionGrid_CPtr selection_grid() const;
#endif
	void set_dicom_texture_set(const SliceTextureSet_Ptr& dicomTextureSet);
#if 1
	void set_ipf_grid(const IPFGrid_Ptr& ipfGrid);
#endif
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
