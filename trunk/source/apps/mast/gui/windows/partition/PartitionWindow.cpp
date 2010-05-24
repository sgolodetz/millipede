/***
 * millipede: PartitionWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionWindow.h"

#include <common/dicom/volumes/Volume.h>
#include <common/dicom/volumes/VolumeTextureSet.h>
#include <mast/util/StringConversion.h>
#include "PartitionCanvas.h"
#include "StratumCanvas.h"

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionWindow::PartitionWindow(wxWindow *parent, const std::string& title, const Volume_Ptr& volume, const VolumeChoice& volumeChoice, wxGLContext *context)
:	wxFrame(parent, -1, string_to_wxString(title), wxDefaultPosition, wxSize(100,100)),
	m_viewedVolume(new ViewedVolume(volume, ViewLocation(0, 0, 0, 0), ORIENT_XY)), m_volumeChoice(volumeChoice)
{
	m_viewedVolume->add_listener(this);

	calculate_canvas_size();
	Show();
	setup_gui(context);

	m_stratumCanvas->setup(m_viewedVolume);
	m_partitionCanvas->setup(m_viewedVolume);
}

//#################### PUBLIC METHODS ####################
wxGLContext *PartitionWindow::get_context() const
{
	return m_stratumCanvas->GetContext();
}

void PartitionWindow::viewed_volume_changed()
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

	Volume::Size volumeSize = m_viewedVolume->volume()->size();
	m_canvasWidth = std::max<int>(512, std::max(volumeSize[0], volumeSize[1]));
	m_canvasHeight = std::max<int>(512, std::max(volumeSize[1], volumeSize[2]));
}

void PartitionWindow::refresh_canvases()
{
	m_stratumCanvas->Refresh();
	m_partitionCanvas->Refresh();
}

}
