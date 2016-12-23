/***
 * millipede: SegmentVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTVOLUMEDIALOG
#define H_MILLIPEDE_SEGMENTVOLUMEDIALOG

#include <boost/lexical_cast.hpp>
#include <boost/optional.hpp>

#include <itkSize.h>

#include <wx/bookctrl.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/propdlg.h>
#include <wx/radiobox.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>

#include <mast/util/StringConversion.h>

//#################### FORWARD DECLARATIONS ####################
class wxPanel;
class wxRadioBox;
class wxSpinCtrl;

namespace mp {

namespace mp_SegmentVolumeDialog {

//#################### ENUMERATIONS ####################
enum
{
	RADIOBOXID_SEGMENTATIONTYPE,
	SPINID_GRIDSIZE,
};

enum SegmentationType
{
	SEGTYPE_XY,
	SEGTYPE_XZ,
	SEGTYPE_YZ,
	SEGTYPE_3D,
	SEGTYPE_CUSTOM,
	SEGTYPE_COUNT,	// dummy value containing the number of segmentation types
};

template <typename SegmentationOptionsType>
class SegmentVolumeDialog : public wxPropertySheetDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	itk::Size<3> m_volumeSize;

	wxRadioBox *m_segmentationType;
	wxSpinCtrl *m_subvolumeSizes[3];

	//#################### PROTECTED VARIABLES ####################
protected:
	boost::optional<SegmentationOptionsType> m_segmentationOptions;

	//#################### CONSTRUCTORS ####################
public:
	explicit SegmentVolumeDialog(const itk::Size<3>& volumeSize)
	:	m_volumeSize(volumeSize)
	{}

	//#################### DESTRUCTOR ####################
protected:
	~SegmentVolumeDialog()
	{}

	//#################### PRIVATE ABSTRACT METHODS ####################
private:
	virtual bool construct_segmentation_options() = 0;
	virtual wxPanel *create_advanced_page(wxWindow *parent) = 0;

	//#################### PUBLIC METHODS ####################
public:
	const boost::optional<SegmentationOptionsType>& segmentation_options() const
	{
		return m_segmentationOptions;
	}

	//#################### PROTECTED METHODS ####################
protected:
	bool construct_subvolume_size(itk::Size<3>& subvolumeSize)
	{
		for(int i=0; i<3; ++i)
		{
			subvolumeSize[i] = m_subvolumeSizes[i]->GetValue();

			if(m_volumeSize[i] % subvolumeSize[i] != 0)
			{
				wxMessageBox(wxT("Error: The subvolume dimensions must be factors of the volume dimensions."), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
				return false;
			}
		}
		return true;
	}

	void initialise(wxWindow *parent, const std::string& title)
	{
		// Note:	This code can't be put in SegmentVolumeDialog's constructor because it calls a virtual method (create_modality_page).
		//			It must therefore be placed here, and derived classes must call initialise(parent); in their constructors.

		Create(parent, wxID_ANY, string_to_wxString(title), wxDefaultPosition, wxDefaultSize);

		wxBookCtrlBase *notebook = GetBookCtrl();
		wxPanel *basicPage = create_basic_page(notebook);
		wxPanel *advancedPage = create_advanced_page(notebook);
		notebook->AddPage(basicPage, wxT("Basic"), true);
		if(advancedPage) notebook->AddPage(advancedPage, wxT("Advanced"), false);

		CreateButtons();
		LayoutDialog();
	}

	const itk::Size<3>& volume_size() const
	{
		return m_volumeSize;
	}

	//#################### PRIVATE METHODS ####################
	wxPanel *create_basic_page(wxWindow *parent)
	{
		wxPanel *panel = new wxPanel(parent);

		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		panel->SetSizer(sizer);

		// Set up the radio box to choose a segmentation type.
		wxString strings[SEGTYPE_COUNT];
		strings[SEGTYPE_XY] = wxT("Segment as &Axial (X-Y) Slices");
		strings[SEGTYPE_XZ] = wxT("Segment as &Coronal (X-Z) Slices");
		strings[SEGTYPE_YZ] = wxT("Segment as &Sagittal (Y-Z) Slices");
		strings[SEGTYPE_3D] = wxT("Segment as 3D &Volume");
		strings[SEGTYPE_CUSTOM] = wxT("Segment using C&ustomised Sub-Volume Size");
		m_segmentationType = new wxRadioBox(panel, RADIOBOXID_SEGMENTATIONTYPE, wxT("Segmentation Type"), wxDefaultPosition, wxDefaultSize, SEGTYPE_COUNT, strings, 1, wxRA_SPECIFY_COLS);
		m_segmentationType->SetSelection(SEGTYPE_3D);
		sizer->Add(m_segmentationType, 0, wxALIGN_CENTRE_HORIZONTAL);

		sizer->AddSpacer(10);

		// Set up the subvolume size options.
		wxStaticBoxSizer *subvolumeSizeOptions = new wxStaticBoxSizer(wxVERTICAL, panel, wxT("Sub-Volume Size"));
		sizer->Add(subvolumeSizeOptions);

		wxPanel *subvolumePanel = new wxPanel(panel);
		subvolumeSizeOptions->Add(subvolumePanel);
		wxGridSizer *subvolumeSizer = new wxGridSizer(0, 2, 0, 0);
		subvolumePanel->SetSizer(subvolumeSizer);

		wxString captions[] = {"X Size:", "Y Size:", "Z Size:"};
		for(int i=0; i<3; ++i)
		{
			subvolumeSizer->Add(new wxStaticText(subvolumePanel, wxID_ANY, captions[i]), 0, wxALIGN_CENTRE_VERTICAL);
			int initial = m_volumeSize[i];
			wxString initialValue = string_to_wxString(boost::lexical_cast<std::string>(initial));
			m_subvolumeSizes[i] = new wxSpinCtrl(subvolumePanel, SPINID_GRIDSIZE, initialValue, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 1, m_volumeSize[i], initial);
			subvolumeSizer->Add(m_subvolumeSizes[i], 0, wxALIGN_CENTRE_VERTICAL);
		}

		sizer->Fit(panel);
		return panel;
	}

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonOK(wxCommandEvent&)
	{
		if(construct_segmentation_options())
		{
			Close();
		}
	}

	//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
	void OnRadioBoxSegmentationType(wxCommandEvent&)
	{
		if(m_segmentationType->GetSelection() == SEGTYPE_CUSTOM) return;

		itk::Size<3> subvolumeSize = m_volumeSize;
		switch(m_segmentationType->GetSelection())
		{
			case SEGTYPE_XY:	subvolumeSize[2] = 1; break;
			case SEGTYPE_XZ:	subvolumeSize[1] = 1; break;
			case SEGTYPE_YZ:	subvolumeSize[0] = 1; break;
			default:			break;
		}

		for(int i=0; i<3; ++i) m_subvolumeSizes[i]->SetValue(subvolumeSize[i]);
	}

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateGridSizeControl(wxUpdateUIEvent& e)
	{
		// Enable the grid size controls iff custom segmentation is selected.
		e.Enable(m_segmentationType->GetSelection() == SEGTYPE_CUSTOM);
	}

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE_TEMPLATE1(SegmentVolumeDialog, wxPropertySheetDialog, SegmentationOptionsType)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(wxID_OK, SegmentVolumeDialog<SegmentationOptionsType>::OnButtonOK)

	//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
	EVT_RADIOBOX(RADIOBOXID_SEGMENTATIONTYPE, SegmentVolumeDialog<SegmentationOptionsType>::OnRadioBoxSegmentationType)

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(SPINID_GRIDSIZE, SegmentVolumeDialog<SegmentationOptionsType>::OnUpdateGridSizeControl)
END_EVENT_TABLE()

}

using mp_SegmentVolumeDialog::SegmentVolumeDialog;

}

#endif
