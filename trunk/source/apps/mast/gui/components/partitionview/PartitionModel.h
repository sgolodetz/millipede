/***
 * millipede: PartitionModel.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONMODEL
#define H_MILLIPEDE_PARTITIONMODEL

#include <vector>

#include <boost/shared_ptr.hpp>

#include <common/commands/UndoableCommandManager.h>
#include <common/dicom/volumes/DICOMVolume.h>
#include <common/partitionforests/images/VolumeIPF.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>
#include <common/partitionforests/images/VolumeIPFSelection.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

template <typename LeafLayer, typename BranchLayer, typename Feature>
class PartitionModel
{
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
	ICommandManager_Ptr m_commandManager;
	DICOMVolume_Ptr m_dicomVolume;
	VolumeIPFMultiFeatureSelection_Ptr m_multiFeatureSelection;
	VolumeIPFSelection_Ptr m_selection;
	VolumeIPF_Ptr m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	explicit PartitionModel(const DICOMVolume_Ptr& dicomVolume)
	:	m_commandManager(new UndoableCommandManager), m_dicomVolume(dicomVolume)
	{}

	//#################### PUBLIC METHODS ####################
public:
	/**
	Calculates the total volume of the voxels marked as the specified feature (in cubic mm).

	@param[in]	feature		The feature whose volume is to be calculated
	@pre
		-	dicom_volume() is non-null
		-	multi_feature_selection() is non-null
		-	volume_ipf() is non-null
	@return	As described
	*/
	double calculate_feature_volume_mm3(const Feature& feature) const
	{
		typedef PartitionForestSelection<LeafLayer,BranchLayer> SelectionT;
		typedef boost::shared_ptr<const SelectionT> Selection_CPtr;
		Selection_CPtr featureSelection = multi_feature_selection()->selection(feature);

		itk::Vector<double,3> spacing = m_dicomVolume->spacing();
		double voxelVolume = spacing[0] * spacing[1] * spacing[2];

		double volume = 0;
		for(typename SelectionT::NodeConstIterator it=featureSelection->nodes_cbegin(), iend=featureSelection->nodes_cend(); it!=iend; ++it)
		{
			const PFNodeID& node = *it;
			if(node.layer() > 0)	volume += m_volumeIPF->branch_properties(node).voxel_count() * voxelVolume;
			else					volume += voxelVolume;
		}
		return volume;
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

	const VolumeIPFSelection_Ptr& selection()
	{
		return m_selection;
	}

	VolumeIPFSelection_CPtr selection() const
	{
		return m_selection;
	}

	void set_command_manager(const ICommandManager_Ptr& commandManager)
	{
		m_commandManager = commandManager;
		if(m_volumeIPF) m_volumeIPF->set_command_manager(commandManager);
		if(m_selection) m_selection->set_command_manager(commandManager);
		if(m_multiFeatureSelection) m_multiFeatureSelection->set_command_manager(commandManager);
	}

	void set_volume_ipf(const VolumeIPF_Ptr& volumeIPF)
	{
		m_commandManager->clear_history();

		m_volumeIPF = volumeIPF;
		m_selection.reset(new VolumeIPFSelectionT(volumeIPF));
		m_multiFeatureSelection.reset(new VolumeIPFMultiFeatureSelectionT(volumeIPF));

		volumeIPF->add_weak_listener(m_selection);
		set_command_manager(m_commandManager);
	}

	const VolumeIPF_Ptr& volume_ipf()
	{
		return m_volumeIPF;
	}

	VolumeIPF_CPtr volume_ipf() const
	{
		return m_volumeIPF;
	}
};

}

#endif
