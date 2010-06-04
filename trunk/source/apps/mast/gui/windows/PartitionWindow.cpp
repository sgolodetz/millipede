/***
 * millipede: PartitionWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionWindow.h"

#include <wx/sizer.h>

#include <mast/gui/components/partitionview/PartitionView.h>
#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionWindow::PartitionWindow(wxWindow *parent, const std::string& title, const DICOMVolume_Ptr& volume, const DICOMVolumeChoice& volumeChoice,
								 wxGLContext *context)
:	wxFrame(parent, -1, string_to_wxString(title), wxDefaultPosition, wxSize(100,100))
{
	SetBackgroundColour(wxColour(240,240,240));

	wxGridSizer *sizer = new wxGridSizer(1, 1, 0, 0);
	SetSizer(sizer);

	Show();

	m_view = new PartitionView(this, volume, volumeChoice, context);
	sizer->Add(m_view);

	sizer->Fit(this);
}

//#################### PUBLIC METHODS ####################
wxGLContext *PartitionWindow::get_context() const
{
	return m_view->get_context();
}

}
