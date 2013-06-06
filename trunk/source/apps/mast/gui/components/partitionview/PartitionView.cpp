/***
 * millipede: PartitionView.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionView.h"

#include <set>

#include <boost/tuple/tuple.hpp>

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/numdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>

#include <common/commands/ListenerAlertingCommandSequenceGuard.h>
#include <common/dicom/volumes/DICOMVolume.h>
#include <common/partitionforests/base/PartitionForestTouchListener.h>
#include <common/partitionforests/images/MosaicImageCreator.h>
#include <common/partitionforests/images/MosaicTextureSetUpdater.h>
#include <common/slices/SliceTextureSetFiller.h>
#include <mast/drawingtools/BoxDrawingTool.h>
#include <mast/drawingtools/LassoDrawingTool.h>
#include <mast/drawingtools/LineLoopDrawingTool.h>
#include <mast/gui/dialogs/DialogUtil.h>
#include <mast/gui/dialogs/DifferenceDialog.h>
#include <mast/gui/overlays/HighlightNodesOverlay.h>
#include <mast/gui/overlays/IPFMultiFeatureSelectionOverlay.h>
#include <mast/gui/overlays/IPFSelectionOverlay.h>
#include <mast/gui/overlays/PartitionOverlayManager.h>
#include <mast/util/StringConversion.h>
#include <mast/models/IPFSetListener.h>
#include <mast/models/DifferenceOptions.h>
#include "DICOMCanvas.h"
#include "PartitionCamera.h"
#include "PartitionCanvas.h"

#include <common/analysis/DICOMIPFDifference.h>
using namespace mp;

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	ID_BASE = wxID_HIGHEST + 1000,	// a dummy value which is never used: subsequent values are guaranteed to be higher than this
	BUTTONID_SEGMENT_VOLUME,
	BUTTONID_VIEW_XY,
	BUTTONID_VIEW_XZ,
	BUTTONID_VIEW_YZ,
	BUTTONID_SEGMENT_MULTIPLE,
	BUTTONID_VISUALIZE_IN_3D,
	BUTTONID_DIFFERENCEU,
	BUTTONID_DIFFERENCEN,
	BUTTONID_DIFFERENCEAB,
	BUTTONID_DIFFERENCEBA,
	BUTTONID_DIFFERENCEDIALOG,
	CHOICEID_DRAWING_TOOL,
	CHOICEID_MULTI_FEATURE_SELECTION,
	SLIDERID_X,
	SLIDERID_Y,
	SLIDERID_Z,
	SLIDERID_LAYER,
	SLIDERID_ZOOM,
	RADIOID_CHOOSEIPF,
};

}

namespace mp {
	
//#################### LISTENERS ####################
struct PartitionView::CameraListener : PartitionCamera::Listener
{
	PartitionView *base;

	explicit CameraListener(PartitionView *base_)
	:	base(base_)
	{}

	void slice_location_changed(bool sliceChanged, bool layerChanged)
	{
		//std::cout << "updating sliders" << std::endl;
		base->update_sliders();
		if(sliceChanged || layerChanged)
		{
			//std::cout << "recreating overlays" << std::endl;
			base->recreate_overlays();
			//std::cout << "resetting drawing tool" << std::endl;
			base->current_drawing_tool()->reset();
		}
		//std::cout << "refreshing canvasses" << std::endl;
		base->refresh_canvases();
		//std::cout << "done" << std::endl;
	}

	void slice_orientation_changed()
	{
		base->recreate_overlays();
		base->current_drawing_tool()->reset();
		base->refresh_canvases();
	}

	void zoom_level_changed()
	{
		base->update_sliders();
		base->current_drawing_tool()->reset();
		base->refresh_canvases();
	}
};

struct PartitionView::IPFListListener : IPFSetListener
{
	PartitionView *base;
	
	explicit IPFListListener(PartitionView *base_)
	:	base(base_)
	{}
	
	void generated(unsigned n)
	{
		std::cout << "" << std::endl;
		//std::cout << "ipfsetlistener.generated" << std::endl;
		base->add_ipf_options(n);
		//std::cout << "added options panel" << std::endl;
		
		base->cache_partition_textures(n);
		std::cout << "cached textures" << std::endl;
	}
	
	void swapped(unsigned i) {
		//std::cout << "ipfsetlistener.swapped" << std::endl;
		base->use_partiton_textures(i);
		//std::cout << "used textures" << std::endl;
	}
};

struct PartitionView::ForestTouchListener : PartitionForestTouchListener<LeafLayer,BranchLayer>
{
	typedef PartitionForestTouchListener<LeafLayer,BranchLayer> Super;

	typedef std::set<int> Layer;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	PartitionView *base;
	VolumeIPF_CPtr volumeIPF;

	explicit ForestTouchListener(PartitionView *base_, const VolumeIPF_CPtr& volumeIPF_)
	:	Super(volumeIPF_->highest_layer()), base(base_), volumeIPF(volumeIPF_)
	{}

	void layer_was_cloned(int index)
	{
		Super::layer_was_cloned(index);

		// Clone the partition texture set.
		Greyscale8SliceTextureSet_Ptr clonedTextureSet(new Greyscale8SliceTextureSet(*base->m_partitionTextureSets[index-1]));
		base->m_partitionTextureSets.insert(base->m_partitionTextureSets.begin() + index, clonedTextureSet);

		// Update the layer slider and camera ranges.
		base->m_layerSlider->SetRange(base->m_layerSlider->GetMin(), base->m_layerSlider->GetMax() + 1);
		base->camera()->set_highest_layer(volumeIPF->highest_layer());

		// Switch to the clone layer.
		SliceLocation loc = base->camera()->slice_location();
		loc.layer = index + 1;
		base->camera()->set_slice_location(loc);
	}

	void layer_was_deleted(int index)
	{
		Super::layer_was_deleted(index);

		// Delete the partition texture set.
		base->m_partitionTextureSets.erase(base->m_partitionTextureSets.begin() + (index - 1));

		// Unless the branch layer we're viewing is the lowest, switch down a layer.
		SliceLocation loc = base->camera()->slice_location();
		if(loc.layer > 1)
		{
			--loc.layer;
			base->camera()->set_slice_location(loc);
		}

		// Update the layer slider and camera ranges.
		base->m_layerSlider->SetRange(base->m_layerSlider->GetMin(), base->m_layerSlider->GetMax() - 1);
		base->camera()->set_highest_layer(volumeIPF->highest_layer());

		// Refresh the canvases.
		base->refresh_canvases();
	}

	void layer_was_undeleted(int index)
	{
		Super::layer_was_undeleted(index);

		// Recreate the partition texture set.
		base->m_partitionTextureSets.insert(base->m_partitionTextureSets.begin() + (index - 1), Greyscale8SliceTextureSet_Ptr(new Greyscale8SliceTextureSet));

		CompositeJob_Ptr job(new CompositeJob);

		typedef MosaicImageCreator<LeafLayer,BranchLayer> MIC;
		typedef SliceTextureSetFiller<unsigned char> TSF;

		for(int i=0; i<3; ++i)
		{
			SliceOrientation ori = SliceOrientation(i);
			if(base->m_dicomTextureSet->has_textures(ori))
			{
				MIC *mosaicImageCreator = new MIC(volumeIPF, index, ori, true);
				TSF *textureSetFiller = new TSF(ori, volumeIPF->volume_size(), base->m_partitionTextureSets[index-1]);
				textureSetFiller->set_volume_image_hook(mosaicImageCreator->get_mosaic_image_hook());
				job->add_subjob(mosaicImageCreator);
				job->add_subjob(textureSetFiller);
			}
		}

		execute_with_progress_dialog(job, base, "Recreating Partition Texture Set", false);

		// Update the layer slider and camera ranges.
		base->m_layerSlider->SetRange(base->m_layerSlider->GetMin(), base->m_layerSlider->GetMax() + 1);
		base->camera()->set_highest_layer(volumeIPF->highest_layer());

		// Switch to the undeleted layer.
		SliceLocation loc = base->camera()->slice_location();
		loc.layer = index;
		base->camera()->set_slice_location(loc);
	}

	void nodes_were_touched(const std::vector<Layer>& nodes)
	{
		CompositeJob_Ptr job(new CompositeJob);
		for(int layer=0, layerCount=static_cast<int>(nodes.size()); layer<layerCount; ++layer)
		{
			if(nodes[layer].empty()) continue;

			for(int i=0; i<3; ++i)
			{
				SliceOrientation ori = SliceOrientation(i);
				if(base->m_dicomTextureSet->has_textures(ori))
				{
					typedef MosaicTextureSetUpdater<LeafLayer,BranchLayer> MosaicTextureSetUpdaterT;
					job->add_subjob(new MosaicTextureSetUpdaterT(base->m_partitionTextureSets[layer-1], layer, nodes[layer], volumeIPF, ori, true));
				}
			}
		}
		Job::execute_managed(job);

		base->recreate_overlays();
		base->refresh_canvases();
	}
};

struct PartitionView::MFSManagerListener : PartitionModelT::PartitionForestMFSManagerT::Listener
{
	PartitionView *base;

	explicit MFSManagerListener(PartitionView *base_)
	:	base(base_)
	{}

	void multi_feature_selection_manager_changed()
	{
		base->add_mfs_listener();
		base->recreate_multi_feature_selection_choice();
		base->recreate_multi_feature_selection_overlay();
		base->refresh_canvases();
	}
};

struct PartitionView::ModelListener : PartitionModelT::Listener
{
	PartitionView *base;

	explicit ModelListener(PartitionView *base_)
	:	base(base_)
	{}

	void forest_changed()
	{
		base->create_partition_textures();
		base->recreate_multi_feature_selection_choice();
		base->recreate_overlays();
		base->current_drawing_tool()->reset();
		base->refresh_canvases();
		base->add_listeners();
	}
};

struct PartitionView::MultiFeatureSelectionListener : PartitionModelT::VolumeIPFMultiFeatureSelectionT::Listener
{
	PartitionView *base;

	explicit MultiFeatureSelectionListener(PartitionView *base_)
	:	base(base_)
	{}

	void multi_feature_selection_changed(int commandDepth)
	{
		if(commandDepth == 0)
		{
			base->recreate_multi_feature_selection_overlay();
			base->refresh_canvases();
		}
	}
};

struct PartitionView::NodeSplitManagerListener : NodeSplitManager<LeafLayer,BranchLayer>::Listener
{
	PartitionView *base;

	explicit NodeSplitManagerListener(PartitionView *base_)
	:	base(base_)
	{}

	void node_split_manager_changed()
	{
		base->recreate_node_split_overlay();
		base->refresh_canvases();
	}
};

struct PartitionView::ParentSwitchManagerListener : ParentSwitchManager<LeafLayer,BranchLayer>::Listener
{
	PartitionView *base;

	explicit ParentSwitchManagerListener(PartitionView *base_)
	:	base(base_)
	{}

	void parent_switch_manager_changed()
	{
		base->recreate_parent_switch_overlay();
		base->refresh_canvases();
	}
};

struct PartitionView::SelectionListener : PartitionModelT::VolumeIPFSelectionT::Listener
{
	PartitionView *base;

	explicit SelectionListener(PartitionView *base_)
	:	base(base_)
	{}

	void selection_changed(int commandDepth)
	{
		if(commandDepth == 0)
		{
			base->recreate_selection_overlay();
			base->refresh_canvases();
		}
	}
};

//#################### CONSTRUCTORS ####################
PartitionView::PartitionView(wxWindow *parent, const PartitionModel_Ptr& model, const ICommandManager_Ptr& commandManager, wxGLContext *context)
:	wxPanel(parent, wxID_ANY, wxDefaultPosition, wxSize(100,100)),
	m_camera(new PartitionCamera(
		initial_slice_location(model->dicom_volume_choice()),
		ORIENT_XY,
		model->dicom_volume()->size()
	)),
	m_commandManager(commandManager),
	m_model(model),
	m_overlayManager(new PartitionOverlayManager)
{
	m_camera->add_shared_listener(boost::shared_ptr<CameraListener>(new CameraListener(this)));
	m_model->add_shared_listener(boost::shared_ptr<ModelListener>(new ModelListener(this)));
	m_model->set_command_manager(commandManager);
	m_model->add_ipfset_listener(new IPFListListener(this));
	
	m_partitionTextureSetCache = new std::vector<TexSet>();

	setup_drawing_tools();

	calculate_canvas_size();
	
	setup_gui(context);

	m_dicomCanvas->setup(this);
	m_partitionCanvas->setup(this);

	fit_image_to_view();
	create_dicom_textures();
	create_overlays();
}

//#################### PUBLIC METHODS ####################
const PartitionCamera_Ptr& PartitionView::camera()
{
	return m_camera;
}

PartitionCamera_CPtr PartitionView::camera() const
{
	return m_camera;
}

void PartitionView::clone_current_layer()
{
	m_model->volume_ipf()->clone_layer(camera()->slice_location().layer);
}

void PartitionView::delete_current_layer()
{
	// Note:	We clear the selection before deleting the layer, since the interaction between the two causes intricacies
	//			that aren't worth solving.
	typedef ListenerAlertingCommandSequenceGuard2<PartitionModelT::VolumeIPFT::Listener, PartitionModelT::VolumeIPFSelectionT::Listener> SequenceGuard;
	SequenceGuard guard(m_commandManager, "Delete Layer", m_model->volume_ipf()->listeners(), m_model->selection()->listeners());
	m_model->selection()->clear();
	m_model->volume_ipf()->delete_layer(camera()->slice_location().layer);
}

void PartitionView::fit_image_to_view()
{
	m_dicomCanvas->fit_image_to_canvas();
}

wxGLContext *PartitionView::get_context() const
{
	return m_dicomCanvas->GetContext();
}

void PartitionView::goto_slice()
{
	wxSlider *slider = NULL;
	switch(m_camera->slice_orientation())
	{
		case ORIENT_XY:	slider = m_zSlider; break;
		case ORIENT_XZ:	slider = m_ySlider; break;
		case ORIENT_YZ:	slider = m_xSlider; break;
	}
	assert(slider != NULL);

	SliceLocation loc = m_camera->slice_location();
	long minValue = slider->GetMin(), maxValue = slider->GetMax();
	long curValue = minValue + loc[m_camera->slice_orientation()];
	long newValue = wxGetNumberFromUser(wxT(""), wxT("Slice Number:"), wxT("Goto Slice"), curValue, minValue, maxValue, this);
	if(newValue != -1)
	{
		loc[m_camera->slice_orientation()] = newValue - minValue;
		m_camera->set_slice_location(loc);
	}
}

SliceLocation PartitionView::initial_slice_location(const DICOMVolumeChoice& volumeChoice)
{
	return SliceLocation((volumeChoice.maxX - volumeChoice.minX)/2, (volumeChoice.maxY - volumeChoice.minY)/2, (volumeChoice.maxZ - volumeChoice.minZ)/2, 0);
}

void PartitionView::merge_selected_nodes()
{
	int mergeLayer = m_model->selection()->merge_layer(m_camera->slice_location().layer);
	std::set<PFNodeID> mergees(m_model->selection()->view_at_layer_cbegin(mergeLayer), m_model->selection()->view_at_layer_cend(mergeLayer));
	m_model->volume_ipf()->merge_nonsibling_nodes(mergees);
}

const PartitionView::PartitionModel_Ptr& PartitionView::model()
{
	return m_model;
}

PartitionView::PartitionModel_CPtr PartitionView::model() const
{
	return m_model;
}

const PartitionView::NodeSplitManager_Ptr& PartitionView::node_split_manager()
{
	return m_nodeSplitManager;
}

const PartitionView::ParentSwitchManager_Ptr& PartitionView::parent_switch_manager()
{
	return m_parentSwitchManager;
}

void PartitionView::unzip_selected_node()
{
	int layerIndex = m_camera->slice_location().layer;
	PFNodeID node = *m_model->selection()->view_at_layer_cbegin(layerIndex);

	int highestLayer = m_model->volume_ipf()->highest_layer();
	long toLayer = wxGetNumberFromUser(wxT(""), wxT("To Layer:"), wxT("Unzip Selected Node"), highestLayer, node.layer() + 1, highestLayer, this);

	if(toLayer != -1)
	{
		m_model->volume_ipf()->unzip_node(node, toLayer);
		m_camera->goto_layer(toLayer);
	}
}

//#################### PRIVATE METHODS ####################
void PartitionView::add_listeners()
{
	m_model->volume_ipf()->add_shared_listener(boost::shared_ptr<ForestTouchListener>(new ForestTouchListener(this, m_model->volume_ipf())));
	m_model->selection()->add_shared_listener(boost::shared_ptr<SelectionListener>(new SelectionListener(this)));
	m_model->multi_feature_selection_manager()->add_shared_listener(boost::shared_ptr<MFSManagerListener>(new MFSManagerListener(this)));
	add_mfs_listener();

	m_nodeSplitManager.reset(new NodeSplitManagerT(m_model->volume_ipf(), m_model->selection()));
	m_model->volume_ipf()->add_weak_listener(m_nodeSplitManager);
	m_nodeSplitManager->add_shared_listener(boost::shared_ptr<NodeSplitManagerListener>(new NodeSplitManagerListener(this)));

	m_parentSwitchManager.reset(new ParentSwitchManagerT(m_model->volume_ipf(), m_model->selection(), m_commandManager));
	m_model->volume_ipf()->add_weak_listener(m_parentSwitchManager);
	m_parentSwitchManager->add_shared_listener(boost::shared_ptr<ParentSwitchManagerListener>(new ParentSwitchManagerListener(this)));
}

void PartitionView::add_mfs_listener()
{
	m_multiFeatureSelectionListener.reset(new MultiFeatureSelectionListener(this));
	m_model->active_multi_feature_selection()->add_weak_listener(m_multiFeatureSelectionListener);
}

void PartitionView::calculate_canvas_size()
{
	// We want our canvases to be at least 512x512, but beyond that their size should be dictated by the sizes
	// of the images. We want to be able to show the images in axial (X-Y), sagittal (Y-Z) and coronal (X-Z)
	// orientations, which dictates which dimensions we need to take into account for the canvas sizes.

	itk::Size<3> volumeSize = m_model->dicom_volume()->size();
	m_canvasWidth = std::max<int>(512, std::max(volumeSize[0], volumeSize[1]));
	m_canvasHeight = std::max<int>(512, std::max(volumeSize[1], volumeSize[2]));
}

void PartitionView::create_dicom_textures()
{
	m_dicomTextureSet.reset(new Greyscale8SliceTextureSet);

	DICOMVolume::WindowedImagePointer windowedImage = m_model->dicom_volume()->windowed_image(volume_choice().windowSettings);
	Job_Ptr job = fill_dicom_textures_job(m_camera->slice_orientation(), windowedImage);
	execute_with_progress_dialog(job, this, "Creating DICOM Texture Set", false);
}

void PartitionView::create_overlays()
{
	m_overlayManager->clear_overlays();
	m_overlayManager->insert_overlay_at_top("IPFMultiFeatureSelection", multi_feature_selection_overlay());
	m_overlayManager->insert_overlay_at_top("IPFSelection", selection_overlay());
	m_overlayManager->insert_overlay_at_top("NodeSplit", node_split_overlay());
	m_overlayManager->insert_overlay_at_top("ParentSwitch", parent_switch_overlay());
}

void PartitionView::use_partiton_textures(unsigned i)
{
	typedef VolumeIPF<DICOMImageLeafLayer,DICOMImageBranchLayer> CTVolumeIPF;
	typedef boost::shared_ptr<const CTVolumeIPF> CTVolumeIPF_CPtr;

	CTVolumeIPF_CPtr volumeIPF = m_model->volume_ipf();
	if(!volumeIPF) return;
	int highestLayer = volumeIPF->highest_layer();

	//std::cout << "choosing texture set " << i << std::endl;
	m_partitionTextureSets = (*m_partitionTextureSetCache)[i];
	//std::cout << "&(*m_partitionTextureSetCache)[i] = " << &(*m_partitionTextureSetCache)[i] << std::endl;

	/*Job_Ptr job = reload_texture_job(m_camera->slice_orientation());
	execute_with_progress_dialog(job, this, "Reloading Partition Texture Sets", false);*/

	m_layerSlider->SetRange(1, highestLayer);
	m_camera->set_highest_layer(highestLayer);
	SliceLocation loc = m_camera->slice_location();
	
	//std::cout << "refreshing" << std::endl;
	refresh_canvases();
	
	if (m_camera->getLayer() > highestLayer) {
		m_camera->set_slice_location(SliceLocation(loc.x, loc.y, loc.z, highestLayer));
	}
}

void PartitionView::create_partition_textures()
{
	//std::cout << "in create_partition_textures" << std::endl;
	typedef VolumeIPF<DICOMImageLeafLayer,DICOMImageBranchLayer> CTVolumeIPF;
	typedef boost::shared_ptr<const CTVolumeIPF> CTVolumeIPF_CPtr;

	CTVolumeIPF_CPtr volumeIPF = m_model->volume_ipf();
	if(!volumeIPF) return;
	int highestLayer = volumeIPF->highest_layer();

	m_partitionTextureSets = std::vector<Greyscale8SliceTextureSet_Ptr>(highestLayer);
	for(int layer=1; layer<=highestLayer; ++layer) m_partitionTextureSets[layer-1].reset(new Greyscale8SliceTextureSet);

	Job_Ptr job = fill_partition_textures_job(m_camera->slice_orientation());
	execute_with_progress_dialog(job, this, "Creating Partition Texture Sets", false);

	m_layerSlider->SetRange(1, highestLayer);
	m_camera->set_highest_layer(highestLayer);
	SliceLocation loc = m_camera->slice_location();
	m_camera->set_slice_location(SliceLocation(loc.x, loc.y, loc.z, (1+highestLayer)/2));
}

void PartitionView::cache_partition_textures(unsigned n)
{
	typedef VolumeIPF<DICOMImageLeafLayer,DICOMImageBranchLayer> CTVolumeIPF;
	typedef boost::shared_ptr<const CTVolumeIPF> CTVolumeIPF_CPtr;


	for (unsigned i = 0; i < n; i++) {
		
		
	//	std::cout << "finding ipf " << i << std::endl;
		CTVolumeIPF_CPtr volumeIPF = m_model->volume_ipf(i);
	//	std::cout << "checking " << std::endl;
		if(!volumeIPF) return;
	//	std::cout << "finding highest_layer " << std::endl;
		
		int highestLayer = volumeIPF->highest_layer();
		
	//	std::cout << "creating texture set " << i << std::endl;
		m_partitionTextureSetCache->push_back(std::vector<Greyscale8SliceTextureSet_Ptr>(highestLayer));
	//	std::cout << "resetting layers" << std::endl;
		for(int layer=1; layer<=highestLayer; ++layer) (((*m_partitionTextureSetCache)[i])[layer-1]).reset(new Greyscale8SliceTextureSet);

	//	std::cout << "making job" << std::endl;
		Job_Ptr job = cache_partition_textures_job(m_camera->slice_orientation(), i, volumeIPF);
	//	std::cout << "executing cache job" << std::endl;
		execute_with_progress_dialog(job, this, "Caching Partition Texture Sets", false);
	}
	
	use_partiton_textures(0);
}

DrawingTool_Ptr PartitionView::current_drawing_tool()
{
	return m_currentDrawingTool.second;
}

Greyscale8SliceTextureSet_CPtr PartitionView::dicom_texture_set() const
{
	return m_dicomTextureSet;
}

Job_Ptr PartitionView::fill_dicom_textures_job(SliceOrientation ori, const itk::Image<unsigned char,3>::Pointer& windowedImage) const
{
	SliceTextureSetFiller<unsigned char> *job = new SliceTextureSetFiller<unsigned char>(ori, m_model->dicom_volume()->size(), m_dicomTextureSet);
	job->set_volume_image(windowedImage);
	return Job_Ptr(job);
}

Job_Ptr PartitionView::fill_partition_textures_job(SliceOrientation ori) const
{
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	VolumeIPF_CPtr volumeIPF = m_model->volume_ipf();
	int highestLayer = volumeIPF->highest_layer();

	CompositeJob_Ptr job(new CompositeJob);
	for(int layer=1; layer<=highestLayer; ++layer)
	{
		typedef MosaicImageCreator<LeafLayer,BranchLayer> MIC;
		typedef SliceTextureSetFiller<unsigned char> TSF;

		MIC *mosaicImageCreator = new MIC(volumeIPF, layer, ori, true);
		TSF *textureSetFiller = new TSF(ori, volumeIPF->volume_size(), m_partitionTextureSets[layer-1]);
		textureSetFiller->set_volume_image_hook(mosaicImageCreator->get_mosaic_image_hook());

		job->add_subjob(mosaicImageCreator);
		job->add_subjob(textureSetFiller);
	}
	return job;
}
Job_Ptr PartitionView::cache_partition_textures_job(SliceOrientation ori, unsigned i, VolumeIPF_CPtr volumeIPF) const
{

	//	std::cout << "creating cache job" << std::endl;
	int highestLayer = volumeIPF->highest_layer();

	CompositeJob_Ptr job(new CompositeJob);
	for(int layer=1; layer<=highestLayer; ++layer)
	{
		typedef MosaicImageCreator<LeafLayer,BranchLayer> MIC;
		typedef SliceTextureSetFiller<unsigned char> TSF;

		MIC *mosaicImageCreator = new MIC(volumeIPF, layer, ori, true);
		TSF *textureSetFiller = new TSF(ori, volumeIPF->volume_size(), ((*m_partitionTextureSetCache)[i][layer-1]));
		textureSetFiller->set_volume_image_hook(mosaicImageCreator->get_mosaic_image_hook());

		job->add_subjob(mosaicImageCreator);
		job->add_subjob(textureSetFiller);
	}
	return job;
}

/*Job_Ptr PartitionView::reload_texture_job(SliceOrientation ori) const
{
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;

	VolumeIPF_CPtr volumeIPF = m_model->volume_ipf();
	int highestLayer = volumeIPF->highest_layer();

	CompositeJob_Ptr job(new CompositeJob);
	for(int layer=1; layer<=highestLayer; ++layer)
	{
		typedef SliceTextureSetFiller<unsigned char> TSF;

		TSF *textureSetFiller = new TSF(ori, volumeIPF->volume_size(), m_partitionTextureSets[layer-1]);
		textureSetFiller->set_volume_image_hook(DataHook<itk::Image<unsigned char,3>::Pointer>(m_partitionTextureSets[layer - 1]));

		job->add_subjob(textureSetFiller);
	}
	return job;
}*/

void PartitionView::fill_textures(SliceOrientation ori)
{
	CompositeJob_Ptr job(new CompositeJob);

	if(!m_dicomTextureSet->has_textures(ori))
	{
		DICOMVolume::WindowedImagePointer windowedImage = m_model->dicom_volume()->windowed_image(volume_choice().windowSettings);
		job->add_subjob(fill_dicom_textures_job(ori, windowedImage));
	}

	if(!m_partitionTextureSets.empty() && !m_partitionTextureSets[0]->has_textures(ori))
	{
		job->add_subjob(fill_partition_textures_job(ori));
	}

	if(!job->empty()) execute_with_progress_dialog(job, this, "Creating Textures", false);
}

PartitionOverlay *PartitionView::multi_feature_selection_overlay() const
{
	PartitionModelT::VolumeIPFMultiFeatureSelection_CPtr multiFeatureSelection = m_model->active_multi_feature_selection();
	if(multiFeatureSelection)
	{
		SliceLocation loc = m_camera->slice_location();
		SliceOrientation ori = m_camera->slice_orientation();
		return new IPFMultiFeatureSelectionOverlay(multiFeatureSelection, loc, ori, feature_colour_map<AbdominalFeature::Enum>());
	}
	else return NULL;
}

std::pair<wxArrayString,int> PartitionView::multi_feature_selection_strings() const
{
	wxArrayString mfsStrings;
	int activeIndex = wxNOT_FOUND;

	typedef PartitionModelT::PartitionForestMFSManager_CPtr MFSManager_CPtr;
	MFSManager_CPtr mfsManager = m_model->multi_feature_selection_manager();

	if(mfsManager)
	{
		typedef PartitionModelT::PartitionForestMFSManagerT::MFSMap MFSMap;
		for(MFSMap::const_iterator it=mfsManager->multi_feature_selections().begin(), iend=mfsManager->multi_feature_selections().end(); it!=iend; ++it)
		{
			if(it->second == mfsManager->active_multi_feature_selection())
			{
				activeIndex = mfsStrings.GetCount();
			}
			mfsStrings.Add(string_to_wxString(it->first));
		}
	}
	else
	{
		mfsStrings.Add(wxT("<None>"));
		activeIndex = 0;
	}

	return std::make_pair(mfsStrings, activeIndex);
}

PartitionOverlay *PartitionView::node_split_overlay() const
{
	PartitionModelT::VolumeIPF_CPtr volumeIPF = m_model->volume_ipf();
	if(volumeIPF && m_nodeSplitManager)
	{
		SliceLocation loc = m_camera->slice_location();
		SliceOrientation ori = m_camera->slice_orientation();

		NodeHighlightSets nhs;

		// TODO: Add more colours as desired (depending on how many subgroups there are likely to be).
		RGBA32 colours[] =
		{
			ITKImageUtil::make_rgba32(255,255,0,50),
			ITKImageUtil::make_rgba32(255,0,255,50),
			ITKImageUtil::make_rgba32(0,255,255,50),
			ITKImageUtil::make_rgba32(0,255,0,50),
			ITKImageUtil::make_rgba32(192,0,0,50)
		};

		const std::list<std::set<PFNodeID> >& subgroups = m_nodeSplitManager->subgroups();
		int curColour = 0, colourCount = sizeof(colours) / sizeof(RGBA32);
		for(std::list<std::set<PFNodeID> >::const_iterator it=subgroups.begin(), iend=subgroups.end(); it!=iend; ++it)
		{
			nhs.push_back(std::make_pair(*it, colours[curColour]));
			curColour = (curColour+1) % colourCount;
		}

		nhs.push_back(std::make_pair(m_nodeSplitManager->unallocated_children(), ITKImageUtil::make_rgba32(0,0,255,50)));

		return new HighlightNodesOverlay(nhs, volumeIPF, loc, ori);
	}
	else return NULL;
}

PartitionOverlayManager_CPtr PartitionView::overlay_manager() const
{
	return m_overlayManager;
}

PartitionOverlay *PartitionView::parent_switch_overlay() const
{
	PartitionModelT::VolumeIPF_CPtr volumeIPF = m_model->volume_ipf();
	if(volumeIPF && m_parentSwitchManager)
	{
		SliceLocation loc = m_camera->slice_location();
		SliceOrientation ori = m_camera->slice_orientation();
		NodeHighlightSets nhs;
		nhs.push_back(std::make_pair(m_parentSwitchManager->potential_new_parents(), ITKImageUtil::make_rgba32(0,255,0,50)));
		return new HighlightNodesOverlay(nhs, volumeIPF, loc, ori);
	}
	else return NULL;
}

Greyscale8SliceTextureSet_CPtr PartitionView::partition_texture_set(int layer) const
{
	int n = layer - 1;
	if(0 <= n && n < static_cast<int>(m_partitionTextureSets.size())) return m_partitionTextureSets[n];
	else return Greyscale8SliceTextureSet_CPtr();
}

void PartitionView::recreate_multi_feature_selection_choice()
{
	m_multiFeatureSelectionChoice->Clear();
	wxArrayString mfsStrings;
	int activeIndex;
	boost::tie(mfsStrings, activeIndex) = multi_feature_selection_strings();
	for(size_t i=0, count=mfsStrings.GetCount(); i<count; ++i)
	{
		m_multiFeatureSelectionChoice->Append(mfsStrings[i]);
	}
	m_multiFeatureSelectionChoice->SetSelection(activeIndex);
}

void PartitionView::recreate_multi_feature_selection_overlay()
{
	m_overlayManager->replace_overlay("IPFMultiFeatureSelection", multi_feature_selection_overlay());
}

void PartitionView::recreate_node_split_overlay()
{
	m_overlayManager->replace_overlay("NodeSplit", node_split_overlay());
}

void PartitionView::recreate_overlays()
{
	recreate_multi_feature_selection_overlay();
	recreate_node_split_overlay();
	recreate_parent_switch_overlay();
	recreate_selection_overlay();
}

void PartitionView::recreate_parent_switch_overlay()
{
	m_overlayManager->replace_overlay("ParentSwitch", parent_switch_overlay());
}

void PartitionView::recreate_selection_overlay()
{
	//std::cout << "replacing selection overlay" <<std::endl;
	m_overlayManager->replace_overlay("IPFSelection", selection_overlay());
}

void PartitionView::add_ipf_options(unsigned n)
{
	wxBoxSizer * sizer = new wxBoxSizer(wxHORIZONTAL);
	m_ipfoptions->SetSizer(sizer);
	//sizer->Add(new wxStaticText(this, wxID_ANY, wxString::Format(wxT("IPF options %i"), n)));
	
	wxString rbStrings[n];
	for (unsigned i = 0; i < n; i++) {
		rbStrings[i] = wxString::Format(wxT("Method %i"), i + 1);
	}
	m_ipfchoice = new wxRadioBox(m_ipfoptions, RADIOID_CHOOSEIPF, wxT("Segmentation displayed"), wxDefaultPosition, wxDefaultSize, n, rbStrings, 1, wxRA_SPECIFY_ROWS);
	m_ipfchoice->SetSelection(0);
	sizer->Add(m_ipfchoice);
	
	
	sizer->Fit(m_ipfoptions);
	/*wxBoxSizer * topLeftSizer = new wxBoxSizer(wxHORIZONTAL);
	
	m_segmentVolumeButton = new wxButton(m_topLeftPanel, BUTTONID_SEGMENT_VOLUME, wxT("Segment Volume"));
	
	topLeftSizer->Add(m_segmentVolumeButton, 0, wxALIGN_CENTRE_HORIZONTAL);
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_SEGMENT_MULTIPLE, wxT("Segment Multiple")));
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_DIFFERENCEAB, wxT("A - B")));
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_DIFFERENCEBA, wxT("B - A")));
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_DIFFERENCEN, wxT("Intersection")));
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_DIFFERENCEU, wxT("Union")));
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_DIFFERENCEDIALOG, wxT("Dialog")));
	
	m_topLeftPanel->SetSizer(topLeftSizer);*/
}

void PartitionView::refresh_canvases()
{
	m_dicomCanvas->Refresh();
	m_partitionCanvas->Refresh();
}

PartitionOverlay *PartitionView::selection_overlay() const
{
	
	//std::cout << "getting selection overlay" <<std::endl;
	PartitionModelT::VolumeIPFSelection_CPtr selection = m_model->selection();
	if(selection)
	{
		//std::cout << "found selection, getting location + orientation" <<std::endl;
		SliceLocation loc = m_camera->slice_location();
		SliceOrientation ori = m_camera->slice_orientation();
		//std::cout << "making overlay" << std::endl;
		PartitionOverlay * ipfso = new IPFSelectionOverlay(selection, loc, ori);
		//std::cout << "returning overlay" << std::endl;
		return ipfso;
	}
	else return NULL;
}

void PartitionView::setup_drawing_tools()
{
	m_drawingTools[DRAWINGTOOL_BOX] = std::make_pair(DRAWINGTOOL_BOX, DrawingTool_Ptr(new BoxDrawingTool));
	m_drawingTools[DRAWINGTOOL_LASSO] = std::make_pair(DRAWINGTOOL_LASSO, DrawingTool_Ptr(new LassoDrawingTool));
	m_drawingTools[DRAWINGTOOL_LINELOOP] = std::make_pair(DRAWINGTOOL_LINELOOP, DrawingTool_Ptr(new LineLoopDrawingTool));
	// TODO: Other drawing tools.

	m_currentDrawingTool = m_drawingTools[DRAWINGTOOL_LASSO];
}

void PartitionView::setup_gui(wxGLContext *context)
{
	SetBackgroundColour(wxColour(240,240,240));

	wxFlexGridSizer *sizer = new wxFlexGridSizer(3, 3, 10, 10);
	SetSizer(sizer);

	int attribList[] =
	{
		WX_GL_RGBA,
		WX_GL_DEPTH_SIZE,
		16,
		WX_GL_DOUBLEBUFFER,
		0
	};

	//std::cout << "top left" << std::endl;
	
	// Top left
	wxBoxSizer * topLeftSizer = new wxBoxSizer(wxHORIZONTAL);
	
	m_topLeftPanel = new wxPanel(this);
	m_topLeftPanel->SetSizer(topLeftSizer);
	
	m_segmentVolumeButton = new wxButton(m_topLeftPanel, BUTTONID_SEGMENT_VOLUME, wxT("Segment Volume"));
	
	topLeftSizer->Add(m_segmentVolumeButton, 0, wxALIGN_CENTRE_HORIZONTAL);
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_SEGMENT_MULTIPLE, wxT("Segment Multiple")));
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_DIFFERENCEAB, wxT("A - B")));
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_DIFFERENCEBA, wxT("B - A")));
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_DIFFERENCEN, wxT("Intersection")));
	topLeftSizer->Add(new wxButton(m_topLeftPanel, BUTTONID_DIFFERENCEU, wxT("Union")));
	
	sizer->Add(m_topLeftPanel);
	
	//std::cout << "top left" << std::endl;
	// Top middle
	sizer->Add(new wxPanel(this));

	// Top right
	wxFlexGridSizer *topRightSizer = new wxFlexGridSizer(1, 0, 0, 5);
		topRightSizer->Add(new wxStaticText(this, wxID_ANY, wxT("Feature Selection:")), 0, wxALIGN_CENTRE_VERTICAL);

		m_multiFeatureSelectionChoice = new wxChoice(this, CHOICEID_MULTI_FEATURE_SELECTION, wxDefaultPosition, wxSize(150,25), wxArrayString());
		recreate_multi_feature_selection_choice();
		topRightSizer->Add(m_multiFeatureSelectionChoice);
	sizer->Add(topRightSizer, 0, wxALIGN_CENTRE_HORIZONTAL);

	// Middle left
	m_dicomCanvas = new DICOMCanvas(this, context, attribList, wxID_ANY, wxDefaultPosition, wxSize(m_canvasWidth, m_canvasHeight));
	sizer->Add(m_dicomCanvas);

	// Middle
	wxPanel *middle = new wxPanel(this);
	wxBoxSizer *middleSizer = new wxBoxSizer(wxVERTICAL);
	middle->SetSizer(middleSizer);
		wxButton *viewXYButton = new wxButton(middle, BUTTONID_VIEW_XY, wxT("View X-Y (usually Axial)"));
		middleSizer->Add(viewXYButton, 0, wxALIGN_CENTRE_HORIZONTAL);

		wxButton *viewXZButton = new wxButton(middle, BUTTONID_VIEW_XZ, wxT("View X-Z (usually Coronal)"));
		middleSizer->Add(viewXZButton, 0, wxALIGN_CENTRE_HORIZONTAL);

		wxButton *viewYZButton = new wxButton(middle, BUTTONID_VIEW_YZ, wxT("View Y-Z (usually Sagittal)"));
		middleSizer->Add(viewYZButton, 0, wxALIGN_CENTRE_HORIZONTAL);

		middleSizer->AddSpacer(10);

		wxButton *visualizeIn3DButton = new wxButton(middle, BUTTONID_VISUALIZE_IN_3D, wxT("Visualize in 3D..."));
		middleSizer->Add(visualizeIn3DButton, 0, wxALIGN_CENTRE_HORIZONTAL);

		middleSizer->AddSpacer(20);

		wxPanel *locationControls = new wxPanel(middle);
		wxFlexGridSizer *locationControlsSizer = new wxFlexGridSizer(0, 2, 0, 0);
		locationControls->SetSizer(locationControlsSizer);
			wxStaticText *xText = new wxStaticText(locationControls, wxID_ANY, wxT("X: "));
			locationControlsSizer->Add(xText, 0, wxALIGN_CENTRE_VERTICAL);
			m_xSlider = new wxSlider(locationControls, SLIDERID_X, volume_choice().minX + m_camera->slice_location().x, volume_choice().minX, volume_choice().maxX, wxDefaultPosition, wxSize(150,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			locationControlsSizer->Add(m_xSlider, 0, wxALIGN_CENTRE);

			wxStaticText *yText = new wxStaticText(locationControls, wxID_ANY, wxT("Y: "));
			locationControlsSizer->Add(yText, 0, wxALIGN_CENTRE_VERTICAL);
			m_ySlider = new wxSlider(locationControls, SLIDERID_Y, volume_choice().minY + m_camera->slice_location().y, volume_choice().minY, volume_choice().maxY, wxDefaultPosition, wxSize(150,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			locationControlsSizer->Add(m_ySlider, 0, wxALIGN_CENTRE);

			wxStaticText *zText = new wxStaticText(locationControls, wxID_ANY, wxT("Z: "));
			locationControlsSizer->Add(zText, 0, wxALIGN_CENTRE_VERTICAL);
			m_zSlider = new wxSlider(locationControls, SLIDERID_Z, volume_choice().minZ+1 + m_camera->slice_location().z, volume_choice().minZ+1, volume_choice().maxZ+1, wxDefaultPosition, wxSize(150,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			locationControlsSizer->Add(m_zSlider, 0, wxALIGN_CENTRE);

			wxStaticText *zoomText = new wxStaticText(locationControls, wxID_ANY, wxT("Zoom: "));
			locationControlsSizer->Add(zoomText, 0, wxALIGN_CENTRE_VERTICAL);
			m_zoomSlider = new wxSlider(locationControls, SLIDERID_ZOOM, m_camera->zoom_level(), m_camera->min_zoom_level(), m_camera->max_zoom_level(), wxDefaultPosition, wxSize(150,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			locationControlsSizer->Add(m_zoomSlider, 0, wxALIGN_CENTRE);
		middleSizer->Add(locationControls, 0, wxALIGN_CENTRE_HORIZONTAL);
	sizer->Add(middle);

	// Middle right
	m_partitionCanvas = new PartitionCanvas(this, get_context(), attribList, wxID_ANY, wxDefaultPosition, wxSize(m_canvasWidth, m_canvasHeight));
	sizer->Add(m_partitionCanvas);

	// Bottom left
	wxFlexGridSizer *drawingToolsSizer = new wxFlexGridSizer(1, 0, 0, 5);
		drawingToolsSizer->Add(new wxStaticText(this, wxID_ANY, wxT("Drawing Tool:")), 0, wxALIGN_CENTRE_VERTICAL);

		wxString drawingToolTypes[DRAWINGTOOL_COUNT];
		drawingToolTypes[DRAWINGTOOL_BOX] = wxT("Box");
		drawingToolTypes[DRAWINGTOOL_LASSO] = wxT("Lasso");
		drawingToolTypes[DRAWINGTOOL_LINELOOP] = wxT("Line Loop");

		m_drawingToolChoice = new wxChoice(this, CHOICEID_DRAWING_TOOL, wxDefaultPosition, wxDefaultSize, DRAWINGTOOL_COUNT, drawingToolTypes);
		m_drawingToolChoice->SetSelection(1);
		drawingToolsSizer->Add(m_drawingToolChoice, 0, wxALIGN_CENTRE_VERTICAL);
	sizer->Add(drawingToolsSizer, 0, wxALIGN_CENTRE_HORIZONTAL|wxALL, 5);

	// Bottom middle
	m_ipfoptions = new wxPanel(this);
	sizer->Add(m_ipfoptions);

	// Bottom right
	wxFlexGridSizer *bottomRightSizer = new wxFlexGridSizer(0, 2, 0, 0);
		wxStaticText *layerText = new wxStaticText(this, wxID_ANY, wxT("Layer: "));
		bottomRightSizer->Add(layerText, 0, wxALIGN_CENTRE_VERTICAL);
		m_layerSlider = new wxSlider(this, SLIDERID_LAYER, 0, 0, 1, wxDefaultPosition, wxSize(150,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
		bottomRightSizer->Add(m_layerSlider, 0, wxALIGN_CENTRE);
	sizer->Add(bottomRightSizer, 0, wxALIGN_CENTRE_HORIZONTAL);

	sizer->Fit(this);
}

void PartitionView::update_sliders()
{
	SliceLocation loc = m_camera->slice_location();
	m_xSlider->SetValue(m_xSlider->GetMin() + loc.x);
	m_ySlider->SetValue(m_ySlider->GetMin() + loc.y);
	m_zSlider->SetValue(m_zSlider->GetMin() + loc.z);
	m_layerSlider->SetValue(loc.layer);
	m_zoomSlider->SetValue(m_camera->zoom_level());
}

const DICOMVolumeChoice& PartitionView::volume_choice() const
{
	return m_model->dicom_volume_choice();
}

void PartitionView::zoom_to_fit()
{
	m_dicomCanvas->zoom_to_fit();
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
void PartitionView::OnButtonSegmentVolume(wxCommandEvent&)
{
	m_model->segment_volume(this);
}

void PartitionView::OnButtonViewXY(wxCommandEvent&)
{
	fill_textures(ORIENT_XY);
	m_camera->set_slice_orientation(ORIENT_XY);
	zoom_to_fit();
}

void PartitionView::OnButtonViewXZ(wxCommandEvent&)
{
	fill_textures(ORIENT_XZ);
	m_camera->set_slice_orientation(ORIENT_XZ);
	zoom_to_fit();
}

void PartitionView::OnButtonViewYZ(wxCommandEvent&)
{
	fill_textures(ORIENT_YZ);
	m_camera->set_slice_orientation(ORIENT_YZ);
	zoom_to_fit();
}

void PartitionView::OnButtonSegmentMultiple(wxCommandEvent&)
{
	m_model->multiple_segment(this);
}

void PartitionView::OnButtonVisualizeIn3D(wxCommandEvent&)
{
	m_model->visualize_in_3d(this, get_context());
}

typedef VolumeIPFSelection<DICOMImageLeafLayer,DICOMImageBranchLayer> VolumeIPFSelectionT;
typedef boost::shared_ptr<VolumeIPFSelectionT> VolumeIPFSelection_Ptr;
	
void PartitionView::OnButtonDiffIntersection(wxCommandEvent&)
{	
	
	DICOMIPFDifference diff = DICOMIPFDifference(m_model->volume_ipf(0), m_model->volume_ipf(1), m_camera->slice_location().layer, m_camera->slice_location().layer);
	
	//std::cout << "selecting defference" << std::endl;
	
	VolumeIPFSelection_Ptr p = diff.select_intersection();
	
	model()->swap_ipf(0);
	//std::cout << "setting as selection" << std::endl;
	model()->selection()->replace_with_selection(p);
	
	
	//std::cout << "selection set" << std::endl;
	
	//std::cout << "recreating overlays" << std::endl;
	recreate_overlays();
	//std::cout << "refreshing canvases" << std::endl;
	refresh_canvases();
	std::cout << "diff comlete" << std::endl;
}

void PartitionView::OnButtonDiffDialog(wxCommandEvent&)
{	
	
	DifferenceDialog dialog(this, model()->volume_ipf_count());
	dialog.ShowModal();
	
	DifferenceOptions options = *(dialog.get_options());
	
	
	DICOMIPFDifference diff = DICOMIPFDifference(m_model->volume_ipf(options.ipfA), m_model->volume_ipf(options.ipfB), options.layerA, options.layerB);
	
	//std::cout << "selecting defference" << std::endl;
	
	VolumeIPFSelection_Ptr p;
	
	switch(options.type) {
		case 0:
			p = diff.select_differenceAB();
			break;
		case 1:
			p = diff.select_union();
			break;
		case 2:
			p = diff.select_intersection();
			break;
	}
	
	//std::cout << "setting as selection" << std::endl;
	model()->selection()->replace_with_selection(p);
	model()->swap_ipf(options.ipfA);
	
	//std::cout << "selection set" << std::endl;
	
	//std::cout << "recreating overlays" << std::endl;
	recreate_overlays();
	//std::cout << "refreshing canvases" << std::endl;
	refresh_canvases();
	std::cout << "diff comlete" << std::endl;
}

void PartitionView::OnButtonDiffBA(wxCommandEvent&)
{	
	
	DICOMIPFDifference diff = DICOMIPFDifference(m_model->volume_ipf(0), m_model->volume_ipf(1), m_camera->slice_location().layer, m_camera->slice_location().layer);
	
	//std::cout << "selecting defference" << std::endl;
	
	VolumeIPFSelection_Ptr p = diff.select_differenceBA();
	
	
	//std::cout << "setting as selection" << std::endl;
	
	model()->swap_ipf(1);
	model()->selection()->replace_with_selection(p);
	
	//std::cout << "selection set" << std::endl;
	
	//std::cout << "recreating overlays" << std::endl;
	recreate_overlays();
	//std::cout << "refreshing canvases" << std::endl;
	refresh_canvases();
	std::cout << "diff comlete" << std::endl;
}

void PartitionView::OnButtonDiffAB(wxCommandEvent&)
{	
	
	DICOMIPFDifference diff = DICOMIPFDifference(m_model->volume_ipf(0), m_model->volume_ipf(1), m_camera->slice_location().layer, m_camera->slice_location().layer);
	
	//std::cout << "selecting defference" << std::endl;
	
	VolumeIPFSelection_Ptr p = diff.select_differenceAB();
	
	
	//std::cout << "setting as selection" << std::endl;
	model()->swap_ipf(0);
	
	model()->selection()->replace_with_selection(p);
	
	//std::cout << "selection set" << std::endl;
	
	//std::cout << "recreating overlays" << std::endl;
	recreate_overlays();
	//std::cout << "refreshing canvases" << std::endl;
	refresh_canvases();
	std::cout << "diff comlete" << std::endl;
}

void PartitionView::OnButtonDiffUnion(wxCommandEvent&)
{	
	
	DICOMIPFDifference diff = DICOMIPFDifference(m_model->volume_ipf(0), m_model->volume_ipf(1), m_camera->slice_location().layer, m_camera->slice_location().layer);
	
	//std::cout << "selecting defference" << std::endl;
	
	VolumeIPFSelection_Ptr p = diff.select_union();
	model()->swap_ipf(0);
	
	//std::cout << "setting as selection" << std::endl;
	model()->selection()->replace_with_selection(p);
	
	//std::cout << "selection set" << std::endl;
	
	//std::cout << "recreating overlays" << std::endl;
	recreate_overlays();
	//std::cout << "refreshing canvases" << std::endl;
	refresh_canvases();
	std::cout << "diff comlete" << std::endl;
}
//~~~~~~~~~~~~~~~~~~~~ CHOICES ~~~~~~~~~~~~~~~~~~~~
void PartitionView::OnChoiceDrawingTool(wxCommandEvent&)
{
	m_currentDrawingTool = m_drawingTools[m_drawingToolChoice->GetSelection()];
}

void PartitionView::OnChoiceMultiFeatureSelection(wxCommandEvent&)
{
	std::string name = wxString_to_string(m_multiFeatureSelectionChoice->GetStringSelection());
	m_model->multi_feature_selection_manager()->set_active_multi_feature_selection(name);
}

void PartitionView::OnRadioChooseIPF(wxCommandEvent& e) {
	//std::cout << "Radio button pressed" << std::endl;
	m_model->swap_ipf(m_ipfchoice->GetSelection());
}

//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
void PartitionView::OnSliderX(wxScrollEvent&)
{
	SliceLocation loc = m_camera->slice_location();
	m_camera->set_slice_location(SliceLocation(m_xSlider->GetValue() - m_xSlider->GetMin(), loc.y, loc.z, loc.layer));
}

void PartitionView::OnSliderY(wxScrollEvent&)
{
	SliceLocation loc = m_camera->slice_location();
	m_camera->set_slice_location(SliceLocation(loc.x, m_ySlider->GetValue() - m_ySlider->GetMin(), loc.z, loc.layer));
}

void PartitionView::OnSliderZ(wxScrollEvent&)
{
	SliceLocation loc = m_camera->slice_location();
	m_camera->set_slice_location(SliceLocation(loc.x, loc.y, m_zSlider->GetValue() - m_zSlider->GetMin(), loc.layer));
}

void PartitionView::OnSliderLayer(wxScrollEvent&)
{
	SliceLocation loc = m_camera->slice_location();
	m_camera->set_slice_location(SliceLocation(loc.x, loc.y, loc.z, m_layerSlider->GetValue()));
}

void PartitionView::OnSliderZoom(wxScrollEvent&)
{
	m_camera->set_zoom_level(m_zoomSlider->GetValue());
}

//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
void PartitionView::OnUpdateForestNeeder(wxUpdateUIEvent& e)
{
	e.Enable(m_model->volume_ipf());
}

void PartitionView::OnUpdateSliderLayer(wxUpdateUIEvent& e)
{
	e.Enable(partition_texture_set(1).get() != NULL);
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(PartitionView, wxPanel)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_SEGMENT_VOLUME, PartitionView::OnButtonSegmentVolume)
	EVT_BUTTON(BUTTONID_VIEW_XY, PartitionView::OnButtonViewXY)
	EVT_BUTTON(BUTTONID_VIEW_XZ, PartitionView::OnButtonViewXZ)
	EVT_BUTTON(BUTTONID_VIEW_YZ, PartitionView::OnButtonViewYZ)
	EVT_BUTTON(BUTTONID_SEGMENT_MULTIPLE, PartitionView::OnButtonSegmentMultiple)
	EVT_BUTTON(BUTTONID_DIFFERENCEU, PartitionView::OnButtonDiffUnion)
	EVT_BUTTON(BUTTONID_DIFFERENCEN, PartitionView::OnButtonDiffIntersection)
	EVT_BUTTON(BUTTONID_DIFFERENCEAB, PartitionView::OnButtonDiffAB)
	EVT_BUTTON(BUTTONID_DIFFERENCEBA, PartitionView::OnButtonDiffBA)
	EVT_BUTTON(BUTTONID_DIFFERENCEDIALOG, PartitionView::OnButtonDiffDialog)
	EVT_BUTTON(BUTTONID_VISUALIZE_IN_3D, PartitionView::OnButtonVisualizeIn3D)

	//~~~~~~~~~~~~~~~~~~~~ RADIO ~~~~~~~~~~~~~~~~~~~~~~
	EVT_RADIOBOX(RADIOID_CHOOSEIPF, PartitionView::OnRadioChooseIPF)
	
	//~~~~~~~~~~~~~~~~~~~~ CHOICES ~~~~~~~~~~~~~~~~~~~~
	EVT_CHOICE(CHOICEID_DRAWING_TOOL, PartitionView::OnChoiceDrawingTool)
	EVT_CHOICE(CHOICEID_MULTI_FEATURE_SELECTION, PartitionView::OnChoiceMultiFeatureSelection)

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	EVT_COMMAND_SCROLL(SLIDERID_X, PartitionView::OnSliderX)
	EVT_COMMAND_SCROLL(SLIDERID_Y, PartitionView::OnSliderY)
	EVT_COMMAND_SCROLL(SLIDERID_Z, PartitionView::OnSliderZ)
	EVT_COMMAND_SCROLL(SLIDERID_LAYER, PartitionView::OnSliderLayer)
	EVT_COMMAND_SCROLL(SLIDERID_ZOOM, PartitionView::OnSliderZoom)

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(BUTTONID_VISUALIZE_IN_3D, PartitionView::OnUpdateForestNeeder)
	EVT_UPDATE_UI(CHOICEID_DRAWING_TOOL, PartitionView::OnUpdateForestNeeder)
	EVT_UPDATE_UI(CHOICEID_MULTI_FEATURE_SELECTION, PartitionView::OnUpdateForestNeeder)
	EVT_UPDATE_UI(SLIDERID_LAYER, PartitionView::OnUpdateSliderLayer)
END_EVENT_TABLE()

}
