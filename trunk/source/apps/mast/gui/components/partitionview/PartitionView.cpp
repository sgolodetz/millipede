/***
 * millipede: PartitionView.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionView.h"

#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/partitionforests/images/MosaicImageCreator.h>
#include <common/segmentation/CTLowestLayersBuilder.h>
#include <common/segmentation/VolumeIPFBuilder.h>
#include <common/slices/SliceTextureSetFiller.h>
#include <mast/gui/dialogs/DialogUtil.h>
#include <mast/gui/dialogs/SegmentCTVolumeDialog.h>
#include <mast/gui/overlays/IPFMultiFeatureSelectionOverlay.h>
#include <mast/gui/overlays/IPFSelectionOverlay.h>
#include <mast/gui/overlays/PartitionOverlayManager.h>
#include <mast/util/StringConversion.h>
#include "DICOMCanvas.h"
#include "PartitionCanvas.h"
using namespace mp;

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	ID_BASE = wxID_HIGHEST + 1000,	// a dummy value which is never used: subsequent values are guaranteed to be higher than this
	BUTTONID_SEGMENT_CT_VOLUME,
	BUTTONID_VIEW_XY,
	BUTTONID_VIEW_XZ,
	BUTTONID_VIEW_YZ,
	BUTTONID_VISUALIZE_IN_3D,
	SLIDERID_X,
	SLIDERID_Y,
	SLIDERID_Z,
	SLIDERID_LAYER,
};

}

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionView::PartitionView(wxWindow *parent, const DICOMVolume_Ptr& volume, const DICOMVolumeChoice& volumeChoice,
							 const ICommandManager_Ptr& commandManager, wxGLContext *context)
:	wxPanel(parent, wxID_ANY, wxDefaultPosition, wxSize(100,100)),
	m_camera(new PartitionCamera(
		SliceLocation((volumeChoice.maxX - volumeChoice.minX)/2, (volumeChoice.maxY - volumeChoice.minY)/2, (volumeChoice.maxZ - volumeChoice.minZ)/2, 0),
		ORIENT_XY,
		volume->size()
	)),
	m_commandManager(commandManager),
	m_model(new PartitionModelT(volume)),
	m_overlayManager(new PartitionOverlayManager),
	m_volumeChoice(volumeChoice)
{
	m_camera->add_listener(this);
	m_camera->set_command_manager(commandManager);
	m_model->add_listener(this);
	m_model->set_command_manager(commandManager);

	calculate_canvas_size();
	setup_gui(context);

	m_dicomCanvas->setup(this);
	m_partitionCanvas->setup(this);

	create_textures(m_camera->slice_orientation());
}

//#################### PUBLIC METHODS ####################
PartitionCamera_CPtr PartitionView::camera() const
{
	return m_camera;
}

void PartitionView::camera_changed()
{
	// Update sliders.
	SliceLocation loc = m_camera->slice_location();
	m_xSlider->SetValue(m_xSlider->GetMin() + loc.x);
	m_ySlider->SetValue(m_ySlider->GetMin() + loc.y);
	m_zSlider->SetValue(m_zSlider->GetMin() + loc.z);
	m_layerSlider->SetValue(loc.layer);

	recreate_overlays();
	refresh_canvases();
}

wxGLContext *PartitionView::get_context() const
{
	return m_dicomCanvas->GetContext();
}

const PartitionView::PartitionModel_Ptr& PartitionView::model()
{
	return m_model;
}

PartitionView::PartitionModel_CPtr PartitionView::model() const
{
	return m_model;
}

void PartitionView::model_changed()
{
	recreate_overlays();
	refresh_canvases();
}

//#################### PRIVATE METHODS ####################
void PartitionView::calculate_canvas_size()
{
	// We want our canvases to be at least 512x512, but beyond that their size should be dictated by the sizes
	// of the images. We want to be able to show the images in axial (X-Y), sagittal (Y-Z) and coronal (X-Z)
	// orientations, which dictates which dimensions we need to take into account for the canvas sizes.

	DICOMVolume::Size volumeSize = m_model->dicom_volume()->size();
	m_canvasWidth = std::max<int>(512, std::max(volumeSize[0], volumeSize[1]));
	m_canvasHeight = std::max<int>(512, std::max(volumeSize[1], volumeSize[2]));
}

bool PartitionView::create_dicom_textures(SliceOrientation ori)
{
	DICOMVolume::WindowedImagePointer windowedImage = m_model->dicom_volume()->windowed_image(m_volumeChoice.windowSettings);

	SliceTextureSet_Ptr textureSet(new SliceTextureSet);
	shared_ptr<SliceTextureSetFiller<unsigned char> > filler(new SliceTextureSetFiller<unsigned char>(ori, m_model->dicom_volume()->size(), textureSet));
	filler->set_volume_image(windowedImage);
	Job::execute_in_thread(filler);
	if(!show_progress_dialog(this, "Creating Slice Textures", filler)) return false;

	m_camera->set_dicom_texture_set(textureSet);
	return true;
}

void PartitionView::create_partition_textures(SliceOrientation ori)
{
	typedef VolumeIPF<CTImageLeafLayer,CTImageBranchLayer> CTVolumeIPF;
	typedef boost::shared_ptr<const CTVolumeIPF> CTVolumeIPF_CPtr;

	CTVolumeIPF_CPtr volumeIPF = m_model->volume_ipf();
	if(!volumeIPF) return;
	int highestLayer = volumeIPF->highest_layer();

	// Create the partition texture sets.
	std::vector<SliceTextureSet_Ptr> partitionTextureSets(highestLayer);
	CompositeJob_Ptr job(new CompositeJob);
	for(int layer=1; layer<=highestLayer; ++layer)
	{
		partitionTextureSets[layer-1].reset(new SliceTextureSet);

		typedef MosaicImageCreator<CTImageLeafLayer,CTImageBranchLayer> MIC;
		typedef SliceTextureSetFiller<unsigned char> TSF;
		MIC *mosaicImageCreator = new MIC(volumeIPF, layer, ori, true);
		TSF *textureSetFiller = new TSF(ori, volumeIPF->volume_size(), partitionTextureSets[layer-1]);
		textureSetFiller->set_volume_image_hook(mosaicImageCreator->get_mosaic_image_hook());

		job->add_subjob(mosaicImageCreator);
		job->add_subjob(textureSetFiller);
	}
	Job::execute_in_thread(job);
	show_progress_dialog(this, "Creating Partition Texture Sets", job, false);

	m_camera->set_partition_texture_sets(partitionTextureSets);
	m_layerSlider->SetRange(1, highestLayer);
	SliceLocation loc = m_camera->slice_location();
	m_camera->set_slice_location(SliceLocation(loc.x, loc.y, loc.z, (1+highestLayer)/2));
}

bool PartitionView::create_textures(SliceOrientation ori)
{
	if(!create_dicom_textures(ori)) return false;
	create_partition_textures(ori);
	return true;
}

PartitionOverlayManager_CPtr PartitionView::overlay_manager() const
{
	return m_overlayManager;
}

void PartitionView::recreate_overlays()
{
	m_overlayManager->clear_overlays();

	SliceLocation loc = m_camera->slice_location();
	SliceOrientation ori = m_camera->slice_orientation();

	PartitionModelT::VolumeIPFMultiFeatureSelection_CPtr multiFeatureSelection = m_model->multi_feature_selection();
	if(multiFeatureSelection)
	{
		Map<AbdominalFeature,RGBA32> colourMap;
		colourMap.set(AF_KIDNEY, ITKImageUtil::make_rgba32(255,255,0,100));
		colourMap.set(AF_LIVER, ITKImageUtil::make_rgba32(128,0,128,100));
		m_overlayManager->insert_overlay_at_top("IPFMultiFeatureSelection", new IPFMultiFeatureSelectionOverlay(multiFeatureSelection, loc, ori, colourMap));
	}

	PartitionModelT::VolumeIPFSelection_CPtr selection = m_model->selection();
	if(selection)
	{
		m_overlayManager->insert_overlay_at_top("IPFSelection", new IPFSelectionOverlay(selection, loc, ori));
	}
}

void PartitionView::refresh_canvases()
{
	m_dicomCanvas->Refresh();
	m_partitionCanvas->Refresh();
}

void PartitionView::set_slice_location(const SliceLocation& loc)
{
	if(!m_oldSliceLocation) m_oldSliceLocation = m_camera->slice_location();
	m_camera->set_slice_location(loc);
}

void PartitionView::setup_gui(wxGLContext *context)
{
	SetBackgroundColour(wxColour(240,240,240));

	wxFlexGridSizer *sizer = new wxFlexGridSizer(3, 3, 5, 5);
	SetSizer(sizer);

	int attribList[] =
	{
		WX_GL_RGBA,
		WX_GL_DEPTH_SIZE,
		16,
		WX_GL_DOUBLEBUFFER,
		0
	};

	// Top left
	sizer->Add(new wxStaticText(this, wxID_ANY, ""));

	// Top middle
	sizer->Add(new wxStaticText(this, wxID_ANY, ""));

	// Top right
	m_segmentVolumeButton = new wxButton(this, BUTTONID_SEGMENT_CT_VOLUME, wxT("Segment CT Volume..."));
	sizer->Add(m_segmentVolumeButton, 0, wxALIGN_CENTER_HORIZONTAL);

	// Middle left
	wxPanel *middleLeft = new wxPanel(this);
	wxBoxSizer *middleLeftSizer = new wxBoxSizer(wxVERTICAL);
	middleLeft->SetSizer(middleLeftSizer);
		m_dicomCanvas = new DICOMCanvas(middleLeft, context, attribList, wxID_ANY, wxDefaultPosition, wxSize(m_canvasWidth, m_canvasHeight));
		middleLeftSizer->Add(m_dicomCanvas);

		wxPanel *middleLeftBottom = new wxPanel(middleLeft);
		wxFlexGridSizer *middleLeftBottomSizer = new wxFlexGridSizer(0, 2, 0, 0);
		middleLeftBottom->SetSizer(middleLeftBottomSizer);
			wxStaticText *xText = new wxStaticText(middleLeftBottom, wxID_ANY, wxT("X: "));
			middleLeftBottomSizer->Add(xText, 0, wxALIGN_CENTER_VERTICAL);
			m_xSlider = new wxSlider(middleLeftBottom, SLIDERID_X, m_volumeChoice.minX + m_camera->slice_location().x, m_volumeChoice.minX, m_volumeChoice.maxX, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			middleLeftBottomSizer->Add(m_xSlider, 0, wxALIGN_CENTER);

			wxStaticText *yText = new wxStaticText(middleLeftBottom, wxID_ANY, wxT("Y: "));
			middleLeftBottomSizer->Add(yText, 0, wxALIGN_CENTER_VERTICAL);
			m_ySlider = new wxSlider(middleLeftBottom, SLIDERID_Y, m_volumeChoice.minY + m_camera->slice_location().y, m_volumeChoice.minY, m_volumeChoice.maxY, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			middleLeftBottomSizer->Add(m_ySlider, 0, wxALIGN_CENTER);

			wxStaticText *zText = new wxStaticText(middleLeftBottom, wxID_ANY, wxT("Z: "));
			middleLeftBottomSizer->Add(zText, 0, wxALIGN_CENTER_VERTICAL);
			m_zSlider = new wxSlider(middleLeftBottom, SLIDERID_Z, m_volumeChoice.minZ+1 + m_camera->slice_location().z, m_volumeChoice.minZ+1, m_volumeChoice.maxZ+1, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			middleLeftBottomSizer->Add(m_zSlider, 0, wxALIGN_CENTER);
		middleLeftSizer->Add(middleLeftBottom, 0, wxALIGN_CENTER_HORIZONTAL);
	sizer->Add(middleLeft);

	// Middle
	wxPanel *middle = new wxPanel(this);
	wxBoxSizer *middleSizer = new wxBoxSizer(wxVERTICAL);
	middle->SetSizer(middleSizer);
		wxButton *viewXYButton = new wxButton(middle, BUTTONID_VIEW_XY, wxT("View X-Y (usually Axial)"));
		middleSizer->Add(viewXYButton, 0, wxALIGN_CENTER_HORIZONTAL);

		wxButton *viewXZButton = new wxButton(middle, BUTTONID_VIEW_XZ, wxT("View X-Z (usually Coronal)"));
		middleSizer->Add(viewXZButton, 0, wxALIGN_CENTER_HORIZONTAL);

		wxButton *viewYZButton = new wxButton(middle, BUTTONID_VIEW_YZ, wxT("View Y-Z (usually Sagittal)"));
		middleSizer->Add(viewYZButton, 0, wxALIGN_CENTER_HORIZONTAL);

		middleSizer->AddSpacer(10);

		wxButton *visualizeIn3DButton = new wxButton(middle, BUTTONID_VISUALIZE_IN_3D, wxT("Visualize in 3D..."));
		middleSizer->Add(visualizeIn3DButton, 0, wxALIGN_CENTER_HORIZONTAL);
	sizer->Add(middle);

	// Middle right
	wxPanel *middleRight = new wxPanel(this);
	wxBoxSizer *middleRightSizer = new wxBoxSizer(wxVERTICAL);
	middleRight->SetSizer(middleRightSizer);
		m_partitionCanvas = new PartitionCanvas(middleRight, get_context(), attribList, wxID_ANY, wxDefaultPosition, wxSize(m_canvasWidth, m_canvasHeight));
		middleRightSizer->Add(m_partitionCanvas);

		wxPanel *middleRightBottom = new wxPanel(middleRight);
		wxFlexGridSizer *middleRightBottomSizer = new wxFlexGridSizer(0, 2, 0, 0);
		middleRightBottom->SetSizer(middleRightBottomSizer);
			wxStaticText *layerText = new wxStaticText(middleRightBottom, wxID_ANY, wxT("Layer: "));
			middleRightBottomSizer->Add(layerText, 0, wxALIGN_CENTER_VERTICAL);
			m_layerSlider = new wxSlider(middleRightBottom, SLIDERID_LAYER, 0, 0, 1, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			middleRightBottomSizer->Add(m_layerSlider, 0, wxALIGN_CENTER);
		middleRightSizer->Add(middleRightBottom, 0, wxALIGN_CENTER_HORIZONTAL);
	sizer->Add(middleRight);

	sizer->Fit(this);
}

//#################### EVENT HANDLERS ####################
//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
void PartitionView::OnButtonSegmentCTVolume(wxCommandEvent&)
{
	// Display a segment CT volume dialog to allow the user to choose how the segmentation process should work.
	SegmentCTVolumeDialog dialog(this, m_model->dicom_volume()->size(), m_volumeChoice.windowSettings);
	dialog.ShowModal();

	if(dialog.segmentation_options())
	{
		typedef VolumeIPFBuilder<CTLowestLayersBuilder> CTVolumeIPFBuilder;
		typedef CTVolumeIPFBuilder::VolumeIPF_Ptr VolumeIPF_Ptr;

		VolumeIPF_Ptr volumeIPF;
		Job_Ptr job(new CTVolumeIPFBuilder(m_model->dicom_volume(), *dialog.segmentation_options(), volumeIPF));
		Job::execute_in_thread(job);
		if(show_progress_dialog(this, "Segmenting CT Volume", job))
		{
			m_model->set_volume_ipf(volumeIPF);
			create_partition_textures(m_camera->slice_orientation());
		}
	}
}

void PartitionView::OnButtonViewXY(wxCommandEvent&)
{
	if(create_textures(ORIENT_XY)) m_camera->set_slice_orientation(ORIENT_XY);
}

void PartitionView::OnButtonViewXZ(wxCommandEvent&)
{
	if(create_textures(ORIENT_XZ)) m_camera->set_slice_orientation(ORIENT_XZ);
}

void PartitionView::OnButtonViewYZ(wxCommandEvent&)
{
	if(create_textures(ORIENT_YZ)) m_camera->set_slice_orientation(ORIENT_YZ);
}

//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
void PartitionView::OnReleaseSlider(wxScrollEvent&)
{
	SliceLocation loc = m_camera->slice_location();
	if(m_oldSliceLocation && *m_oldSliceLocation != loc)
	{
		m_camera->change_slice_location(*m_oldSliceLocation, loc, "Change Slice Location");
		m_oldSliceLocation = boost::none;
	}
}

void PartitionView::OnTrackSliderX(wxScrollEvent&)
{
	SliceLocation loc = m_camera->slice_location();
	set_slice_location(SliceLocation(m_xSlider->GetValue() - m_xSlider->GetMin(), loc.y, loc.z, loc.layer));
}

void PartitionView::OnTrackSliderY(wxScrollEvent&)
{
	SliceLocation loc = m_camera->slice_location();
	set_slice_location(SliceLocation(loc.x, m_ySlider->GetValue() - m_ySlider->GetMin(), loc.z, loc.layer));
}

void PartitionView::OnTrackSliderZ(wxScrollEvent&)
{
	SliceLocation loc = m_camera->slice_location();
	set_slice_location(SliceLocation(loc.x, loc.y, m_zSlider->GetValue() - m_zSlider->GetMin(), loc.layer));
}

void PartitionView::OnTrackSliderLayer(wxScrollEvent&)
{
	SliceLocation loc = m_camera->slice_location();
	set_slice_location(SliceLocation(loc.x, loc.y, loc.z, m_layerSlider->GetValue()));
}

//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
void PartitionView::OnUpdateSliderX(wxUpdateUIEvent& e)		{ e.Enable(m_camera->slice_orientation() == ORIENT_YZ); }
void PartitionView::OnUpdateSliderY(wxUpdateUIEvent& e)		{ e.Enable(m_camera->slice_orientation() == ORIENT_XZ); }
void PartitionView::OnUpdateSliderZ(wxUpdateUIEvent& e)		{ e.Enable(m_camera->slice_orientation() == ORIENT_XY); }
void PartitionView::OnUpdateSliderLayer(wxUpdateUIEvent& e)	{ e.Enable(m_camera->partition_texture_set(1).get() != NULL); }

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(PartitionView, wxPanel)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_SEGMENT_CT_VOLUME, PartitionView::OnButtonSegmentCTVolume)
	EVT_BUTTON(BUTTONID_VIEW_XY, PartitionView::OnButtonViewXY)
	EVT_BUTTON(BUTTONID_VIEW_XZ, PartitionView::OnButtonViewXZ)
	EVT_BUTTON(BUTTONID_VIEW_YZ, PartitionView::OnButtonViewYZ)

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	EVT_COMMAND_SCROLL_THUMBRELEASE(SLIDERID_X, PartitionView::OnReleaseSlider)
	EVT_COMMAND_SCROLL_THUMBRELEASE(SLIDERID_Y, PartitionView::OnReleaseSlider)
	EVT_COMMAND_SCROLL_THUMBRELEASE(SLIDERID_Z, PartitionView::OnReleaseSlider)
	EVT_COMMAND_SCROLL_THUMBRELEASE(SLIDERID_LAYER, PartitionView::OnReleaseSlider)
	EVT_COMMAND_SCROLL_THUMBTRACK(SLIDERID_X, PartitionView::OnTrackSliderX)
	EVT_COMMAND_SCROLL_THUMBTRACK(SLIDERID_Y, PartitionView::OnTrackSliderY)
	EVT_COMMAND_SCROLL_THUMBTRACK(SLIDERID_Z, PartitionView::OnTrackSliderZ)
	EVT_COMMAND_SCROLL_THUMBTRACK(SLIDERID_LAYER, PartitionView::OnTrackSliderLayer)

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(SLIDERID_X, PartitionView::OnUpdateSliderX)
	EVT_UPDATE_UI(SLIDERID_Y, PartitionView::OnUpdateSliderY)
	EVT_UPDATE_UI(SLIDERID_Z, PartitionView::OnUpdateSliderZ)
	EVT_UPDATE_UI(SLIDERID_LAYER, PartitionView::OnUpdateSliderLayer)
END_EVENT_TABLE()

}
