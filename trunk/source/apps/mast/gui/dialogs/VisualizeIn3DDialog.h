/***
 * millipede: VisualizeIn3DDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VISUALIZEIN3DDIALOG
#define H_MILLIPEDE_VISUALIZEIN3DDIALOG

#include <boost/optional.hpp>

#include <wx/dialog.h>

#include <common/visualization/VisualizationOptions.h>

//#################### FORWARD DECLARATIONS ####################
class wxCheckBox;
class wxSpinCtrl;
class wxTextCtrl;

namespace mp {

class VisualizeIn3DDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	wxCheckBox *m_laplacianSmoothingCheckBox;
	wxSpinCtrl *m_laplacianSmoothingIterations;
	wxTextCtrl *m_laplacianSmoothingRelaxationFactor;

	wxCheckBox *m_meshDecimationCheckBox;
	wxSpinCtrl *m_meshDecimationReductionTarget;

	boost::optional<VisualizationOptions> m_visualizationOptions;

	//#################### CONSTRUCTORS ####################
public:
	explicit VisualizeIn3DDialog(wxWindow *parent);

	//#################### PUBLIC METHODS ####################
public:
	const boost::optional<VisualizationOptions>& visualization_options() const;

	//#################### PRIVATE METHODS ####################
private:
	bool construct_visualization_options();

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonOK(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateLaplacianSmoothingOption(wxUpdateUIEvent& e);
	void OnUpdateMeshDecimationOption(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif
