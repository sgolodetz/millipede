/***
 * millipede: SegmentVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTVOLUMEDIALOG
#define H_MILLIPEDE_SEGMENTVOLUMEDIALOG

#include <itkSize.h>

#include <wx/propdlg.h>

//#################### FORWARD DECLARATIONS ####################
class wxPanel;
class wxRadioBox;
class wxSpinCtrl;

namespace mp {

class SegmentVolumeDialog : public wxPropertySheetDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	itk::Size<3> m_volumeSize;

	wxSpinCtrl *m_gridSizes[3];
	wxRadioBox *m_segmentationType;

	//#################### CONSTRUCTORS ####################
public:
	explicit SegmentVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize);

	//#################### PRIVATE METHODS ####################
private:
	wxPanel *create_basic_page(wxWindow *parent);
	wxPanel *create_advanced_page(wxWindow *parent);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
	void OnRadioBoxSegmentationType(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateGridSizeControl(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif
