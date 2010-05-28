/***
 * millipede: SegmentCTVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTCTVOLUMEDIALOG
#define H_MILLIPEDE_SEGMENTCTVOLUMEDIALOG

#include <boost/optional.hpp>

#include <itkSize.h>

#include <wx/propdlg.h>

#include <common/segmentation/CTSegmentationOptions.h>

//#################### FORWARD DECLARATIONS ####################
class wxPanel;
class wxRadioBox;
class wxSpinCtrl;

namespace mp {

class SegmentCTVolumeDialog : public wxPropertySheetDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	itk::Size<3> m_volumeSize;
	WindowSettings m_windowSettings;
	boost::optional<CTSegmentationOptions> m_segmentationOptions;

	wxSpinCtrl *m_adfIterations;
	wxRadioBox *m_inputType;
	wxRadioBox *m_segmentationType;
	wxSpinCtrl *m_subvolumeSizes[3];
	wxSpinCtrl *m_waterfallLayerLimit;

	//#################### CONSTRUCTORS ####################
public:
	SegmentCTVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings);

	//#################### PUBLIC METHODS ####################
public:
	const boost::optional<CTSegmentationOptions>& segmentation_options() const;

	//#################### PRIVATE METHODS ####################
private:
	void construct_segmentation_options();
	wxPanel *create_advanced_page(wxWindow *parent);
	wxPanel *create_basic_page(wxWindow *parent);
	wxPanel *create_modality_page(wxWindow *parent);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonOK(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
	void OnRadioBoxSegmentationType(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateGridSizeControl(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif
