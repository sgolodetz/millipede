/***
 * millipede: PartitionModel.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONMODEL
#define H_MILLIPEDE_PARTITIONMODEL

#include <vector>

#include <boost/shared_ptr.hpp>

#include <common/commands/UndoableCommandManager.h>
#include <common/partitionforests/images/VolumeIPF.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>
#include <common/partitionforests/images/VolumeIPFSelection.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class DICOMVolume> DICOMVolume_Ptr;
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

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
