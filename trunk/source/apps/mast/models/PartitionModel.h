/***
 * millipede: PartitionModel.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONMODEL
#define H_MILLIPEDE_PARTITIONMODEL

#include <vector>

#include <boost/shared_ptr.hpp>

#include <common/partitionforests/images/VolumeIPF.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>
#include <common/partitionforests/images/VolumeIPFSelection.h>
#include <common/slices/SliceLocation.h>
#include <common/slices/SliceOrientation.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class DICOMVolume> DICOMVolume_Ptr;
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;
typedef boost::shared_ptr<class SliceTextureSet> SliceTextureSet_Ptr;
typedef boost::shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;

template <typename LeafLayer, typename BranchLayer, typename Feature>
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
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	typedef VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;
	typedef boost::shared_ptr<const VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_CPtr;

	typedef VolumeIPFSelection<LeafLayer,BranchLayer> VolumeIPFSelectionT;
	typedef boost::shared_ptr<VolumeIPFSelectionT> VolumeIPFSelection_Ptr;
	typedef boost::shared_ptr<const VolumeIPFSelectionT> VolumeIPFSelection_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	SliceTextureSet_Ptr m_dicomTextureSet;
	DICOMVolume_Ptr m_dicomVolume;
	VolumeIPFMultiFeatureSelection_Ptr m_multiFeatureSelection;
	std::vector<SliceTextureSet_Ptr> m_partitionTextureSets;
	VolumeIPFSelection_Ptr m_selection;
	SliceLocation m_sliceLocation;			// slice location in terms of the volume only (not based on actual slice numbers)
	SliceOrientation m_sliceOrientation;
	VolumeIPF_Ptr m_volumeIPF;

	std::vector<Listener*> m_listeners;

	//#################### CONSTRUCTORS ####################
public:
	PartitionModel(const DICOMVolume_Ptr& dicomVolume, const SliceLocation& sliceLocation, SliceOrientation sliceOrientation)
	:	m_dicomVolume(dicomVolume), m_sliceLocation(sliceLocation), m_sliceOrientation(sliceOrientation)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void add_listener(Listener *listener)
	{
		m_listeners.push_back(listener);
	}

	SliceTextureSet_CPtr dicom_texture_set() const
	{
		return m_dicomTextureSet;
	}

	DICOMVolume_CPtr dicom_volume() const
	{
		return m_dicomVolume;
	}

	const VolumeIPFMultiFeatureSelection_Ptr& multi_feature_selection()
	{
		return m_multiFeatureSelection;
	}

	VolumeIPFMultiFeatureSelection_CPtr multi_feature_selection() const
	{
		return m_multiFeatureSelection;
	}

	SliceTextureSet_CPtr partition_texture_set(int layer) const
	{
		int n = layer - 1;
		if(0 <= n && n < static_cast<int>(m_partitionTextureSets.size())) return m_partitionTextureSets[n];
		else return SliceTextureSet_CPtr();
	}

	const VolumeIPFSelection_Ptr& selection()
	{
		return m_selection;
	}

	VolumeIPFSelection_CPtr selection() const
	{
		return m_selection;
	}

	void set_dicom_texture_set(const SliceTextureSet_Ptr& dicomTextureSet)
	{
		m_dicomTextureSet = dicomTextureSet;
		alert_listeners();
	}

	void set_partition_texture_sets(const std::vector<SliceTextureSet_Ptr>& partitionTextureSets)
	{
		m_partitionTextureSets = partitionTextureSets;
		alert_listeners();
	}

	void set_slice_location(const SliceLocation& loc)
	{
		// TODO: Validate location against bounds
		m_sliceLocation = loc;
		alert_listeners();
	}

	void set_slice_orientation(SliceOrientation ori)
	{
		m_sliceOrientation = ori;
		alert_listeners();
	}

	void set_volume_ipf(const VolumeIPF_Ptr& volumeIPF)
	{
		m_volumeIPF = volumeIPF;
		m_selection.reset(new VolumeIPFSelectionT(volumeIPF));
		volumeIPF->add_listener(m_selection);
		m_multiFeatureSelection.reset(new VolumeIPFMultiFeatureSelectionT(volumeIPF));

#if 0
		if(m_sliceOrientation == ORIENT_XY)
		{
			m_multiFeatureSelection->identify_feature(m_volumeIPF->node_of(m_volumeIPF->highest_layer(), ITKImageUtil::make_index(60,280,0)), AF_KIDNEY);
			m_multiFeatureSelection->identify_feature(m_volumeIPF->node_of(m_volumeIPF->highest_layer(), ITKImageUtil::make_index(300,150,0)), AF_LIVER);
		}
		else if(m_sliceOrientation == ORIENT_XZ)
		{
			m_selection->select_node(m_volumeIPF->node_of(m_volumeIPF->highest_layer(), ITKImageUtil::make_index(60,0,50)));
		}
#endif

		alert_listeners();
	}

	const SliceLocation& slice_location() const
	{
		return m_sliceLocation;
	}

	SliceOrientation slice_orientation() const
	{
		return m_sliceOrientation;
	}

	const VolumeIPF_Ptr& volume_ipf()
	{
		return m_volumeIPF;
	}

	VolumeIPF_CPtr volume_ipf() const
	{
		return m_volumeIPF;
	}

	//#################### PRIVATE METHODS ####################
private:
	void alert_listeners()
	{
		for(size_t i=0, size=m_listeners.size(); i<size; ++i)
		{
			m_listeners[i]->model_changed();
		}
	}
};

}

#endif
