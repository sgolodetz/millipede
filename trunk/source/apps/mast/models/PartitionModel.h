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
#include <common/dicom/volumes/DICOMVolumeChoice.h>
#include <common/partitionforests/base/PartitionForestMFSManager.h>
#include <common/partitionforests/images/LabelImageCreator.h>
#include <common/partitionforests/images/VolumeIPF.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>
#include <common/partitionforests/images/VolumeIPFSelection.h>
#include <common/segmentation/DICOMLowestLayersBuilder.h>
#include <common/segmentation/VolumeIPFBuilder.h>
#include <common/util/ITKImageUtil.h>
#include <common/visualization/LaplacianSmoother.h>
#include <common/visualization/MeshBuilder.h>
#include <common/visualization/MeshDecimator.h>
#include <common/visualization/MeshRendererCreator.h>
#include <mast/gui/dialogs/DialogUtil.h>
#include <mast/gui/dialogs/SegmentDICOMVolumeDialog.h>
#include <mast/gui/dialogs/SegmentMultipleDialog.h>
#include <mast/gui/dialogs/VisualizeIn3DDialog.h>
#include <mast/gui/windows/VisualizationWindow.h>

#include <mast/models/SegmentationRun.h>
#include <mast/models/IPFSetListener.h>


#include <common/partitionforests/base/PartitionForest.h>

//#################### FORWARD DECLARATIONS ####################
//class wxGLContext;

template <typename LeafLayer, typename BranchLayer, typename Feature> class SegmentationRun;
template <typename LeafLayer, typename BranchLayer, typename Feature> class SegmentMultipleDialog;

namespace mp {

template <typename LeafLayer, typename BranchLayer, typename Feature>
class PartitionModel
{
	//#################### TYPEDEFS ####################
public:
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;
	
	typedef std::vector<VolumeIPF_Ptr> VolumeIPFSet;
	typedef boost::shared_ptr<VolumeIPFSet> VolumeIPFSet_Ptr;

	typedef VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;
	typedef boost::shared_ptr<const VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_CPtr;

	typedef VolumeIPFSelection<LeafLayer,BranchLayer> VolumeIPFSelectionT;
	typedef boost::shared_ptr<VolumeIPFSelectionT> VolumeIPFSelection_Ptr;
	typedef boost::shared_ptr<const VolumeIPFSelectionT> VolumeIPFSelection_CPtr;

	typedef PartitionForestMFSManager<VolumeIPFMultiFeatureSelectionT> PartitionForestMFSManagerT;
	typedef boost::shared_ptr<PartitionForestMFSManagerT> PartitionForestMFSManager_Ptr;
	typedef boost::shared_ptr<const PartitionForestMFSManagerT> PartitionForestMFSManager_CPtr;
	typedef SegmentationRun<LeafLayer, BranchLayer, Feature> SegRun;

	//#################### LISTENERS ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void forest_changed() = 0;
	};

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void forest_changed()
		{
			multicast(boost::bind(&Listener::forest_changed, _1));
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_commandManager;
	DICOMVolume_Ptr m_dicomVolume;
	DICOMVolumeChoice m_dicomVolumeChoice;
	CompositeListener m_listeners;
	IPFSetListener * m_setListener;
	PartitionForestMFSManager_Ptr m_multiFeatureSelectionManager;
	VolumeIPFSelection_Ptr m_selection;
	VolumeIPF_Ptr m_volumeIPF;
	std::vector<VolumeIPF_Ptr> m_volumeIPFs;
	std::vector<SegRun*> *m_runs;

	//#################### CONSTRUCTORS ####################
public:
	PartitionModel(const DICOMVolume_Ptr& dicomVolume, const DICOMVolumeChoice& dicomVolumeChoice)
	:	m_commandManager(new UndoableCommandManager), m_dicomVolume(dicomVolume), m_dicomVolumeChoice(dicomVolumeChoice)
	{}

	//#################### PUBLIC METHODS ####################
public:
	VolumeIPFMultiFeatureSelection_Ptr active_multi_feature_selection()
	{
		return m_multiFeatureSelectionManager ? m_multiFeatureSelectionManager->active_multi_feature_selection() : VolumeIPFMultiFeatureSelection_Ptr();
	}

	VolumeIPFMultiFeatureSelection_CPtr active_multi_feature_selection() const
	{
		return m_multiFeatureSelectionManager ? m_multiFeatureSelectionManager->active_multi_feature_selection() : VolumeIPFMultiFeatureSelection_CPtr();
	}

	void add_shared_listener(const boost::shared_ptr<Listener>& listener)
	{
		m_listeners.add_shared_listener(listener);
	}
	
	void swap_ipf(unsigned n) {
		//std::cout << "Gonna try swap ipfs" << std::endl;
		if (m_runs || m_runs->size() > n) {
			//std::cout << "Swapping to " << n << std::endl;
			//set_volume_ipf();
			
			m_commandManager->clear_history();

			VolumeIPF_Ptr volumeIPF = ((*m_runs)[n])->getVolumeIPF();
			
			m_volumeIPF = volumeIPF;
			m_selection.reset(new VolumeIPFSelectionT(volumeIPF));
			VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection(new VolumeIPFMultiFeatureSelectionT(volumeIPF));
			m_multiFeatureSelectionManager.reset(new PartitionForestMFSManagerT("Default", multiFeatureSelection));

			volumeIPF->add_weak_listener(m_selection);
			set_command_manager(m_commandManager);

			m_setListener->swapped(n);
		}
	}
	
	
	
	
	void add_ipfset_listener(IPFSetListener * listener)
	{
		m_setListener = listener;
	}

	DICOMVolume_CPtr dicom_volume() const
	{
		return m_dicomVolume;
	}

	const DICOMVolumeChoice& dicom_volume_choice() const
	{
		return m_dicomVolumeChoice;
	}

	const PartitionForestMFSManager_Ptr& multi_feature_selection_manager()
	{
		return m_multiFeatureSelectionManager;
	}

	void multiple_segment(wxWindow *parent)
	{
		CompositeJob_Ptr job = CompositeJob_Ptr(new CompositeJob());
		
		
		SegmentMultipleDialog<LeafLayer,BranchLayer,Feature> dialog(parent, m_dicomVolume->size(), m_dicomVolumeChoice.windowSettings, m_dicomVolume);
		dialog.ShowModal();
		
		//std::cout << "Getting runs" << std::endl;
		m_runs = dialog.get_runs();
	
		
		if (!m_runs || m_runs->size() <= 0) {
			std::cout << "No runs" << std::endl;
			return;
		}
		
		//std::cout << "Found runs" << std::endl;
		
		
		m_volumeIPFs = std::vector<VolumeIPF_Ptr>(m_runs->size());
		
		//std::cout << "initialised m_vipfs" << std::endl;
		for (unsigned i = 0; i < m_runs->size(); i++) {
			
			//std::cout << "initialising m_vipfs[i]" << std::endl;
			m_volumeIPFs[i] = boost::shared_ptr<VolumeIPFT>();
			
			VolumeIPF_Ptr volumeIPF;
			//std::cout << "making job" << std::endl;
			//(*m_runs)[i].getJob();
			//(*m_runs)[i].make_job(m_dicomVolume);
			
			//std::cout <<"(*m_runs)[i]->getVolumeIPF() = " << (*m_runs)[i]->getVolumeIPF() << std::endl;
			//std::cout << "Adding run " << i << " to job" << std::endl;
			job->add_subjob((*m_runs)[i]->getJob());
			//std::cout << "Added run " << i << " to job" << std::endl;
			typedef VolumeIPFBuilder<DICOMLowestLayersBuilder> DICOMVolumeIPFBuilder;
			//job->add_subjob(new DICOMVolumeIPFBuilder(m_dicomVolume,*((*m_runs)[i].getOptions()) , volumeIPF));
		}
		
		//std::cout << "Added all runs" << std::endl;
			
		
		//if(!job) return;
		
		if(execute_with_progress_dialog(job, parent, "Performing Segmentations"))
		{
			std::cout << "segmentation complete" << std::endl;
			if ((*m_runs)[0]->getVolumeIPF()) {
				//std::cout << "found ipf" << std::endl;
				set_volume_ipf((*m_runs)[0]->getVolumeIPF());
			}
			
			for (unsigned i = 0; i < m_runs->size(); i++) {
				
				m_volumeIPFs[i] = (*m_runs)[i]->getVolumeIPF();
				
			}
			
			m_setListener->generated(m_runs->size());
		}
	  
	}
	
	void segment_volume(wxWindow *parent)
	{
		VolumeIPF_Ptr volumeIPF;

		Job_Ptr job;

		// Display a segment volume dialog to allow the user to choose how the segmentation process should work.
		SegmentDICOMVolumeDialog dialog(parent, m_dicomVolume->size(), m_dicomVolumeChoice.windowSettings);
		dialog.ShowModal();
		if(dialog.segmentation_options())
		{
			typedef VolumeIPFBuilder<DICOMLowestLayersBuilder> DICOMVolumeIPFBuilder;
			job.reset(new DICOMVolumeIPFBuilder(m_dicomVolume, *dialog.segmentation_options(), volumeIPF));
		}

		// If the user cancelled the segment volume dialog, exit.
		if(!job) return;
		//std::cout << "3. volumeIPF = " << volumeIPF << std::endl;

		// Actually segment the volume. If the segmentation finishes successfully, set the volume IPF accordingly.
		if(execute_with_progress_dialog(job, parent, "Segmenting Volume"))
		{
		//	std::cout << "4. volumeIPF = " << volumeIPF << std::endl;
			set_volume_ipf(volumeIPF);
		}
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
		if(m_multiFeatureSelectionManager) m_multiFeatureSelectionManager->set_command_manager(commandManager);
	}

	void set_volume_ipf(const VolumeIPF_Ptr& volumeIPF)
	{
		m_commandManager->clear_history();

		m_volumeIPF = volumeIPF;
		m_selection.reset(new VolumeIPFSelectionT(volumeIPF));
		VolumeIPFMultiFeatureSelection_Ptr multiFeatureSelection(new VolumeIPFMultiFeatureSelectionT(volumeIPF));
		m_multiFeatureSelectionManager.reset(new PartitionForestMFSManagerT("Default", multiFeatureSelection));

		volumeIPF->add_weak_listener(m_selection);
		set_command_manager(m_commandManager);

		m_listeners.forest_changed();
	}

	void visualize_in_3d(wxWindow *parent, wxGLContext *context)
	{
		// Display a visualize in 3D dialog to allow the user to choose how the visualization process should work.
		VisualizeIn3DDialog dialog(parent);
		dialog.ShowModal();

		// If the user wants to visualize the model, construct the mesh and display it in a separate window.
		if(dialog.visualization_options())
		{
			VisualizationOptions options = *dialog.visualization_options();

			CompositeJob_Ptr job(new CompositeJob);

			// Set up label image creation.
			typedef LabelImageCreator<LeafLayer,BranchLayer,Feature> LabelImageCreatorT;
			LabelImageCreatorT *labelImageCreator = new LabelImageCreatorT(active_multi_feature_selection());
			job->add_subjob(labelImageCreator);

			// Set up mesh building.
			MeshBuilder<int> *meshBuilder = new MeshBuilder<int>(labelImageCreator->labelling_size());
			meshBuilder->set_labelling_hook(labelImageCreator->get_labelling_hook());
			job->add_subjob(meshBuilder);

			// Set up Laplacian smoothing if desired.
			if(options.laplacianSmoothingEnabled)
			{
				LaplacianSmoother<int> *laplacianSmoother = new LaplacianSmoother<int>(options.laplacianSmoothingLambda, options.laplacianSmoothingIterations);
				laplacianSmoother->set_mesh_hook(meshBuilder->get_mesh_hook());
				job->add_subjob(laplacianSmoother);
			}

			// Set up mesh decimation if desired.
			if(options.meshDecimationEnabled)
			{
				MeshDecimator<int> *meshDecimator = new MeshDecimator<int>(options.meshDecimationReductionTarget);
				meshDecimator->set_mesh_hook(meshBuilder->get_mesh_hook());
				job->add_subjob(meshDecimator);
			}

			// Set up the mesh renderer creator.
			std::map<Feature,RGBA32> featureColourMap = feature_colour_map<Feature>();
			std::map<int,RGBA32> submeshColourMap;
			submeshColourMap.insert(std::make_pair(0, ITKImageUtil::make_rgba32(255,0,0,255)));
			for(typename std::map<Feature,RGBA32>::const_iterator it=featureColourMap.begin(), iend=featureColourMap.end(); it!=iend; ++it)
			{
				submeshColourMap.insert(std::make_pair(feature_to_int(it->first), it->second));
			}

			std::vector<Feature> features = enum_values<Feature>();
			std::map<std::string,int> submeshNameMap;
			submeshNameMap.insert(std::make_pair("Internals", 0));
			for(int i=0, size=static_cast<int>(features.size()); i<size; ++i)
			{
				submeshNameMap.insert(std::make_pair(feature_to_name(features[i]), feature_to_int(i)));
			}

			// Note: The mesh in the builder is *shared* with the smoother and decimator (if used), so this mesh hook is the right one.
			MeshRendererCreator *meshRendererCreator = new MeshRendererCreator(meshBuilder->get_mesh_hook(), submeshColourMap, submeshNameMap);
			job->add_subjob(meshRendererCreator);

			if(execute_with_progress_dialog(job, parent, "Building 3D Model"))
			{
				MeshRenderer_Ptr meshRenderer = meshRendererCreator->get_mesh_renderer();
				meshRenderer->set_submesh_enabled("Internals", false);
				std::string caption = "MAST Visualization - " + m_dicomVolumeChoice.description() + " - Untitled";

				// Note: There isn't a memory leak here - wxWidgets cleans up the window internally.
				new VisualizationWindow(parent, caption, meshRenderer, m_dicomVolume->spacing(), context);
			}
		}
	}

	const VolumeIPF_Ptr& volume_ipf()
	{
		return m_volumeIPF;
	}
	
	const unsigned volume_ipf_count()
	{
		return m_volumeIPFs.size();
	}

	const VolumeIPF_Ptr& volume_ipf(unsigned n)
	{
		
		/*for (unsigned i = 0; i < m_volumeIPFs.size(); i++) {
			//std::cout << "&m_volumeIPFs[" << i << "] = " << &(m_volumeIPFs[i]) << std::endl;
			//std::cout << "m_volumeIPFs[" << i << "] = " << m_volumeIPFs[i] << std::endl;
		}
			//std::cout << "&m_volumeIPFs[" << n << "] = " << &(m_volumeIPFs[n]) << std::endl;
			//std::cout << "m_volumeIPFs[" << n << "] = " << m_volumeIPFs[n] << std::endl;
		*/
		//std::cout << "returning ipf" << n << std::endl;
		return m_volumeIPFs[n];
		/*std::cout << "Checking ipf exists" << std::endl;
		if ((*m_runs)[n]) {
			std::cout << "returning ipf" << std::endl;
			return ((*m_runs)[n])->getVolumeIPF();
		}
		std::cout << "Counldn't find ipf " << n << std::endl;
		return ((*m_runs)[n])->getVolumeIPF();*/
	}
	
	VolumeIPF_CPtr volume_ipf() const
	{
		return m_volumeIPF;
	}
};

}

#endif
