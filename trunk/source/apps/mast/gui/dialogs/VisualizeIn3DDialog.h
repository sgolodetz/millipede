/***
 * millipede: VisualizeIn3DDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VISUALIZEIN3DDIALOG
#define H_MILLIPEDE_VISUALIZEIN3DDIALOG

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

namespace mp {

namespace mp_VisualizeIn3DDialog {

//#################### ENUMERATIONS ####################
enum
{
	SPINID_ITERATIONS,
	SPINID_REDUCTIONTARGET,
	TEXTID_RELAXATIONFACTOR,
};

template <typename LeafLayer, typename BranchLayer, typename Feature>
class VisualizeIn3DDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	wxCheckBox *m_laplacianSmoothingCheckBox;
	wxCheckBox *m_meshDecimationCheckBox;

	//#################### CONSTRUCTORS ####################
public:
	explicit VisualizeIn3DDialog(wxWindow *parent)
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
		wxGridSizer *laplacianSizer = new wxGridSizer(0, 2, 0, 0);
		laplacianPanel->SetSizer(laplacianSizer);

		laplacianSizer->Add(new wxStaticText(laplacianPanel, wxID_ANY, wxT("Relaxation Factor:")));
		wxTextCtrl *relaxationFactor = new wxTextCtrl(laplacianPanel, TEXTID_RELAXATIONFACTOR, wxT("0.5"));
		laplacianSizer->Add(relaxationFactor);

		laplacianSizer->Add(new wxStaticText(laplacianPanel, wxID_ANY, wxT("Iterations:")));
		wxSpinCtrl *iterations = new wxSpinCtrl(laplacianPanel, SPINID_ITERATIONS, wxT("6"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 1, 20, 6);
		laplacianSizer->Add(iterations);

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
		wxGridSizer *meshDecimationSizer = new wxGridSizer(0, 2, 0, 0);
		meshDecimationPanel->SetSizer(meshDecimationSizer);

		meshDecimationSizer->Add(new wxStaticText(meshDecimationPanel, wxID_ANY, wxT("Reduction Target (%):")));
		wxSpinCtrl *reductionTarget = new wxSpinCtrl(meshDecimationPanel, SPINID_REDUCTIONTARGET, wxT("85"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 99, 85);
		meshDecimationSizer->Add(reductionTarget);

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

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateLaplacianSmoothingOption(wxUpdateUIEvent& e)
	{
		e.Enable(m_laplacianSmoothingCheckBox->IsChecked());
	}

	void OnUpdateMeshDecimationOption(wxUpdateUIEvent& e)
	{
		e.Enable(m_meshDecimationCheckBox->IsChecked());
	}

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE_TEMPLATE3(VisualizeIn3DDialog, wxDialog, LeafLayer, BranchLayer, Feature)
	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(SPINID_ITERATIONS, (VisualizeIn3DDialog<LeafLayer,BranchLayer,Feature>::OnUpdateLaplacianSmoothingOption))
	EVT_UPDATE_UI(SPINID_REDUCTIONTARGET, (VisualizeIn3DDialog<LeafLayer,BranchLayer,Feature>::OnUpdateMeshDecimationOption))
	EVT_UPDATE_UI(TEXTID_RELAXATIONFACTOR, (VisualizeIn3DDialog<LeafLayer,BranchLayer,Feature>::OnUpdateLaplacianSmoothingOption))
END_EVENT_TABLE()

}

using mp_VisualizeIn3DDialog::VisualizeIn3DDialog;

}

#endif
