/***
 * millipede: VisualizeIn3DDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "VisualizeIn3DDialog.h"

#include <boost/lexical_cast.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	SPINID_ITERATIONS,
	SPINID_REDUCTIONTARGET,
	TEXTID_RELAXATIONFACTOR,
};

}

namespace mp {

//#################### CONSTRUCTORS ####################
VisualizeIn3DDialog::VisualizeIn3DDialog(wxWindow *parent)
:	wxDialog(parent, wxID_ANY, wxT("Visualize In 3D"), wxDefaultPosition, wxDefaultSize)
{
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	SetSizer(sizer);

	// Add the description.
	wxStaticText *description = new wxStaticText(this, wxID_ANY,	wxT("This tool visualizes labelled volumes in 3D using an approach\n") \
																	wxT("called multiple material marching cubes (M3C), developed by\n") \
																	wxT("Wu and Sullivan. The initial mesh generated is generally quite\n") \
																	wxT("stair-stepped and contains far too many triangles, so it needs\n") \
																	wxT("to be smoothed and decimated. These two post-processing\n") \
																	wxT("stages can be controlled using the options below."));
	sizer->Add(description, 0, wxALL, 10);
	sizer->AddSpacer(10);

	// Add the Laplacian smoothing options.
	wxStaticBoxSizer *laplacianSmoothingOptions = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Laplacian Smoothing"));
	sizer->Add(laplacianSmoothingOptions, 0, wxALIGN_CENTRE_HORIZONTAL);

	laplacianSmoothingOptions->AddSpacer(10);

	m_laplacianSmoothingCheckBox = new wxCheckBox(this, wxID_ANY, wxT("Enable"));
	m_laplacianSmoothingCheckBox->SetValue(true);
	laplacianSmoothingOptions->Add(m_laplacianSmoothingCheckBox);

	laplacianSmoothingOptions->AddSpacer(10);

	wxPanel *laplacianPanel = new wxPanel(this);
	laplacianSmoothingOptions->Add(laplacianPanel);
	wxGridSizer *laplacianSizer = new wxGridSizer(0, 2, 5, 5);
	laplacianPanel->SetSizer(laplacianSizer);

	laplacianSizer->Add(new wxStaticText(laplacianPanel, wxID_ANY, wxT("Relaxation Factor (0-1):")), 0, wxALIGN_CENTRE_VERTICAL);
	m_laplacianSmoothingRelaxationFactor = new wxTextCtrl(laplacianPanel, TEXTID_RELAXATIONFACTOR, wxT("0.5"));
	laplacianSizer->Add(m_laplacianSmoothingRelaxationFactor);

	laplacianSizer->Add(new wxStaticText(laplacianPanel, wxID_ANY, wxT("Iterations:")), 0, wxALIGN_CENTRE_VERTICAL);
	m_laplacianSmoothingIterations = new wxSpinCtrl(laplacianPanel, SPINID_ITERATIONS, wxT("6"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 1, 20, 6);
	laplacianSizer->Add(m_laplacianSmoothingIterations);

	sizer->AddSpacer(10);

	// Add the mesh decimation options.
	wxStaticBoxSizer *meshDecimationOptions = new wxStaticBoxSizer(wxVERTICAL, this, wxT("Mesh Decimation"));
	sizer->Add(meshDecimationOptions, 0, wxALIGN_CENTRE_HORIZONTAL);

	meshDecimationOptions->AddSpacer(10);

	m_meshDecimationCheckBox = new wxCheckBox(this, wxID_ANY, wxT("Enable"));
	m_meshDecimationCheckBox->SetValue(true);
	meshDecimationOptions->Add(m_meshDecimationCheckBox);

	meshDecimationOptions->AddSpacer(10);

	wxPanel *meshDecimationPanel = new wxPanel(this);
	meshDecimationOptions->Add(meshDecimationPanel);
	wxGridSizer *meshDecimationSizer = new wxGridSizer(0, 2, 5, 5);
	meshDecimationPanel->SetSizer(meshDecimationSizer);

	meshDecimationSizer->Add(new wxStaticText(meshDecimationPanel, wxID_ANY, wxT("Reduction Target (%):")), 0, wxALIGN_CENTRE_VERTICAL);
	m_meshDecimationReductionTarget = new wxSpinCtrl(meshDecimationPanel, SPINID_REDUCTIONTARGET, wxT("85"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 99, 85);
	meshDecimationSizer->Add(m_meshDecimationReductionTarget);

	sizer->AddSpacer(10);

	// Add the buttons panel.
	wxPanel *buttonsPanel = new wxPanel(this);
	wxBoxSizer *buttonsSizer = new wxBoxSizer(wxHORIZONTAL);
	buttonsPanel->SetSizer(buttonsSizer);
		wxButton *okButton = new wxButton(buttonsPanel, wxID_OK, wxT("OK"));
		buttonsSizer->Add(okButton);
		okButton->SetFocus();

		wxButton *cancelButton = new wxButton(buttonsPanel, wxID_CANCEL, wxT("Cancel"));
		buttonsSizer->Add(cancelButton);
	sizer->Add(buttonsPanel, 0, wxALIGN_CENTRE_HORIZONTAL);

	sizer->Fit(this);
	CentreOnParent();
}

//#################### PUBLIC METHODS ####################
const boost::optional<VisualizationOptions>& VisualizeIn3DDialog::visualization_options() const
{
	return m_visualizationOptions;
}

//#################### PRIVATE METHODS ####################
bool VisualizeIn3DDialog::construct_visualization_options()
{
	double lambda;
	try
	{
		lambda = lexical_cast<double>(m_laplacianSmoothingRelaxationFactor->GetValue());
	}
	catch(bad_lexical_cast&)
	{
		wxMessageBox(wxT("Error: The Laplacian smoothing relaxation factor must be a decimal number."), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
		return false;
	}

	if(lambda < 0 || lambda > 1)
	{
		wxMessageBox(wxT("Error: The Laplacian smoothing relaxation factor must be between 0 and 1."), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
		return false;
	}

	m_visualizationOptions = VisualizationOptions(m_laplacianSmoothingCheckBox->IsChecked(),
												  m_laplacianSmoothingIterations->GetValue(),
												  lambda,
												  m_meshDecimationCheckBox->IsChecked(),
												  m_meshDecimationReductionTarget->GetValue());
	return true;
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
void VisualizeIn3DDialog::OnButtonOK(wxCommandEvent&)
{
	if(construct_visualization_options())
	{
		Close();
	}
}

//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
void VisualizeIn3DDialog::OnUpdateLaplacianSmoothingOption(wxUpdateUIEvent& e)
{
	e.Enable(m_laplacianSmoothingCheckBox->IsChecked());
}

void VisualizeIn3DDialog::OnUpdateMeshDecimationOption(wxUpdateUIEvent& e)
{
	e.Enable(m_meshDecimationCheckBox->IsChecked());
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(VisualizeIn3DDialog, wxDialog)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(wxID_OK, VisualizeIn3DDialog::OnButtonOK)

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(SPINID_ITERATIONS, VisualizeIn3DDialog::OnUpdateLaplacianSmoothingOption)
	EVT_UPDATE_UI(SPINID_REDUCTIONTARGET, VisualizeIn3DDialog::OnUpdateMeshDecimationOption)
	EVT_UPDATE_UI(TEXTID_RELAXATIONFACTOR, VisualizeIn3DDialog::OnUpdateLaplacianSmoothingOption)
END_EVENT_TABLE()

}
