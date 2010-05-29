/***
 * millipede: PartitionWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionWindow.h"

#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/partitionforests/images/MosaicImageCreator.h>
#include <common/segmentation/CTIPFBuilder.h>
#include <common/segmentation/IPFGridBuilder.h>
#include <common/slices/SliceTextureSetCreator.h>
#include <mast/gui/dialogs/SegmentCTVolumeDialog.h>
#include <mast/util/DialogUtil.h>
#include <mast/util/StringConversion.h>
#include "PartitionCanvas.h"
#include "StratumCanvas.h"
using namespace mp;

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	ID_BASE = wxID_HIGHEST,		// a dummy value which is never used: subsequent values are guaranteed to be higher than this
	BUTTONID_SEGMENT_CT_VOLUME,
	BUTTONID_VIEW_XY,
	BUTTONID_VIEW_XZ,
	BUTTONID_VIEW_YZ,
	SLIDERID_X,
	SLIDERID_Y,
	SLIDERID_Z,
	SLIDERID_LAYER,
};

}

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionWindow::PartitionWindow(wxWindow *parent, const std::string& title, const DICOMVolume_Ptr& volume, const DICOMVolumeChoice& volumeChoice,
								 wxGLContext *context)
:	wxFrame(parent, -1, string_to_wxString(title), wxDefaultPosition, wxSize(100,100)),
	m_model(new PartitionModel(
		volume,
		ViewLocation((volumeChoice.maxX - volumeChoice.minX)/2, (volumeChoice.maxY - volumeChoice.minY)/2, (volumeChoice.maxZ - volumeChoice.minZ)/2, 0),
		ORIENT_XY)
	),
	m_volumeChoice(volumeChoice)
{
	m_model->add_listener(this);

	calculate_canvas_size();
	Show();
	setup_gui(context);

	m_stratumCanvas->setup(m_model);
	m_partitionCanvas->setup(m_model);

	create_textures(m_model->slice_orientation());
}

//#################### PUBLIC METHODS ####################
wxGLContext *PartitionWindow::get_context() const
{
	return m_stratumCanvas->GetContext();
}

void PartitionWindow::model_changed()
{
	refresh_canvases();
	// TODO: Update slider values
}

//#################### PRIVATE METHODS ####################
void PartitionWindow::calculate_canvas_size()
{
	// We want our canvases to be at least 512x512, but beyond that their size should be dictated by the sizes
	// of the images. We want to be able to show the images in axial (X-Y), sagittal (Y-Z) and coronal (X-Z)
	// orientations, which dictates which dimensions we need to take into account for the canvas sizes.

	DICOMVolume::Size volumeSize = m_model->dicom_volume()->size();
	m_canvasWidth = std::max<int>(512, std::max(volumeSize[0], volumeSize[1]));
	m_canvasHeight = std::max<int>(512, std::max(volumeSize[1], volumeSize[2]));
}

bool PartitionWindow::create_textures(SliceOrientation ori)
{
	DICOMVolume::WindowedImagePointer windowedImage = m_model->dicom_volume()->windowed_image(m_volumeChoice.windowSettings);

	SliceTextureSet_Ptr textureSet(new SliceTextureSet);
	shared_ptr<Job> textureSetCreator(new SliceTextureSetCreator<unsigned char>(windowedImage, ori, textureSet));
	Job::execute_in_thread(textureSetCreator);
	if(!show_progress_dialog(this, "Creating Slice Textures", textureSetCreator)) return false;

	m_model->set_dicom_texture_set(textureSet);
	return true;
}

void PartitionWindow::refresh_canvases()
{
	m_stratumCanvas->Refresh();
	m_partitionCanvas->Refresh();
}

void PartitionWindow::setup_gui(wxGLContext *context)
{
	SetBackgroundColour(wxColour(240,240,240));

	wxFlexGridSizer *sizer = new wxFlexGridSizer(3, 3, 5, 5);
	SetSizer(sizer);

	int attribList[] =
	{
		WX_GL_RGBA,
		WX_GL_DEPTH_SIZE,
		16,
		WX_GL_DOUBLEBUFFER
	};

	// Top left
	sizer->Add(new wxStaticText(this, wxID_ANY, ""));

	// Top middle
	sizer->Add(new wxStaticText(this, wxID_ANY, ""));

	// Top right
	wxButton *segmentVolumeButton = new wxButton(this, BUTTONID_SEGMENT_CT_VOLUME, wxT("Segment CT Volume..."));
	sizer->Add(segmentVolumeButton, 0, wxALIGN_CENTER_HORIZONTAL);

	// Middle left
	wxPanel *middleLeft = new wxPanel(this);
	wxBoxSizer *middleLeftSizer = new wxBoxSizer(wxVERTICAL);
	middleLeft->SetSizer(middleLeftSizer);
		m_stratumCanvas = new StratumCanvas(middleLeft, context, attribList, wxID_ANY, wxDefaultPosition, wxSize(m_canvasWidth, m_canvasHeight));
		middleLeftSizer->Add(m_stratumCanvas);

		wxPanel *middleLeftBottom = new wxPanel(middleLeft);
		wxFlexGridSizer *middleLeftBottomSizer = new wxFlexGridSizer(0, 2, 0, 0);
		middleLeftBottom->SetSizer(middleLeftBottomSizer);
			wxStaticText *xText = new wxStaticText(middleLeftBottom, wxID_ANY, wxT("X: "));
			middleLeftBottomSizer->Add(xText, 0, wxALIGN_CENTER_VERTICAL);
			m_xSlider = new wxSlider(middleLeftBottom, SLIDERID_X, m_volumeChoice.minX + m_model->view_location().x, m_volumeChoice.minX, m_volumeChoice.maxX, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			middleLeftBottomSizer->Add(m_xSlider, 0, wxALIGN_CENTER);

			wxStaticText *yText = new wxStaticText(middleLeftBottom, wxID_ANY, wxT("Y: "));
			middleLeftBottomSizer->Add(yText, 0, wxALIGN_CENTER_VERTICAL);
			m_ySlider = new wxSlider(middleLeftBottom, SLIDERID_Y, m_volumeChoice.minY + m_model->view_location().y, m_volumeChoice.minY, m_volumeChoice.maxY, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			middleLeftBottomSizer->Add(m_ySlider, 0, wxALIGN_CENTER);

			wxStaticText *zText = new wxStaticText(middleLeftBottom, wxID_ANY, wxT("Z: "));
			middleLeftBottomSizer->Add(zText, 0, wxALIGN_CENTER_VERTICAL);
			m_zSlider = new wxSlider(middleLeftBottom, SLIDERID_Z, m_volumeChoice.minZ+1 + m_model->view_location().z, m_volumeChoice.minZ+1, m_volumeChoice.maxZ+1, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
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
			m_layerSlider = new wxSlider(middleRightBottom, SLIDERID_LAYER, 999, 999, 999, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			middleRightBottomSizer->Add(m_layerSlider, 0, wxALIGN_CENTER);
		middleRightSizer->Add(middleRightBottom, 0, wxALIGN_CENTER_HORIZONTAL);
	sizer->Add(middleRight);

	sizer->Fit(this);
}

//#################### EVENT HANDLERS ####################
//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
void PartitionWindow::OnButtonSegmentCTVolume(wxCommandEvent&)
{
	// Display a segment CT volume dialog to allow the user to choose how the segmentation process should work.
	SegmentCTVolumeDialog dialog(this, m_model->dicom_volume()->size(), m_volumeChoice.windowSettings);
	dialog.ShowModal();

	if(dialog.segmentation_options())
	{
		typedef IPFGridBuilder<CTIPFBuilder> CTIPFGridBuilder;
		typedef CTIPFGridBuilder::IPF CTIPF;
		typedef CTIPFGridBuilder::IPFG_Ptr CTIPFGrid_Ptr;

		boost::shared_ptr<CTIPFGrid_Ptr> ipfGrid(new CTIPFGrid_Ptr);
		std::vector<itk::Image<unsigned char,3>::Pointer> mosaicImages;

		CompositeJob_Ptr job(new CompositeJob);
		job->add_subjob(new CTIPFGridBuilder(m_model->dicom_volume(), *dialog.segmentation_options(), ipfGrid));
		job->add_subjob(new MosaicImageCreator<CTIPF>(ipfGrid, mosaicImages, false));
		Job::execute_in_thread(job);
		show_progress_dialog(this, "Segmenting CT Volume", job);
	}
}

void PartitionWindow::OnButtonViewXY(wxCommandEvent&)
{
	if(create_textures(ORIENT_XY)) m_model->set_slice_orientation(ORIENT_XY);
}

void PartitionWindow::OnButtonViewXZ(wxCommandEvent&)
{
	if(create_textures(ORIENT_XZ)) m_model->set_slice_orientation(ORIENT_XZ);
}

void PartitionWindow::OnButtonViewYZ(wxCommandEvent&)
{
	if(create_textures(ORIENT_YZ)) m_model->set_slice_orientation(ORIENT_YZ);
}

//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
void PartitionWindow::OnSliderX(wxScrollEvent&)
{
	ViewLocation loc = m_model->view_location();
	m_model->set_view_location(ViewLocation(m_xSlider->GetValue() - m_xSlider->GetMin(), loc.y, loc.z, loc.layer));
}

void PartitionWindow::OnSliderY(wxScrollEvent&)
{
	ViewLocation loc = m_model->view_location();
	m_model->set_view_location(ViewLocation(loc.x, m_ySlider->GetValue() - m_ySlider->GetMin(), loc.z, loc.layer));
}

void PartitionWindow::OnSliderZ(wxScrollEvent&)
{
	ViewLocation loc = m_model->view_location();
	m_model->set_view_location(ViewLocation(loc.x, loc.y, m_zSlider->GetValue() - m_zSlider->GetMin(), loc.layer));
}

void PartitionWindow::OnSliderLayer(wxScrollEvent&)
{
	// TODO
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(PartitionWindow, wxFrame)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_SEGMENT_CT_VOLUME, PartitionWindow::OnButtonSegmentCTVolume)
	EVT_BUTTON(BUTTONID_VIEW_XY, PartitionWindow::OnButtonViewXY)
	EVT_BUTTON(BUTTONID_VIEW_XZ, PartitionWindow::OnButtonViewXZ)
	EVT_BUTTON(BUTTONID_VIEW_YZ, PartitionWindow::OnButtonViewYZ)

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	EVT_COMMAND_SCROLL(SLIDERID_X, PartitionWindow::OnSliderX)
	EVT_COMMAND_SCROLL(SLIDERID_Y, PartitionWindow::OnSliderY)
	EVT_COMMAND_SCROLL(SLIDERID_Z, PartitionWindow::OnSliderZ)
	EVT_COMMAND_SCROLL(SLIDERID_LAYER, PartitionWindow::OnSliderLayer)
END_EVENT_TABLE()

}
