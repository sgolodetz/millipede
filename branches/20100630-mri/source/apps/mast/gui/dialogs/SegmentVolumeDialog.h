/***
 * millipede: SegmentVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTVOLUMEDIALOG
#define H_MILLIPEDE_SEGMENTVOLUMEDIALOG

#include <boost/optional.hpp>

#include <itkSize.h>

#include <wx/propdlg.h>

//#################### FORWARD DECLARATIONS ####################
class wxPanel;
class wxRadioBox;
class wxSpinCtrl;

namespace mp {

template <typename SegmentationOptions>
class SegmentVolumeDialog : public wxPropertySheetDialog
{
	//#################### PROTECTED VARIABLES ####################
	// TODO: Make as many of these private as possible later.
protected:
	itk::Size<3> m_volumeSize;
	WindowSettings m_windowSettings;
	boost::optional<SegmentationOptions> m_segmentationOptions;

	wxSpinCtrl *m_adfIterations;
	wxRadioBox *m_segmentationType;
	wxSpinCtrl *m_subvolumeSizes[3];
	wxSpinCtrl *m_waterfallLayerLimit;

	//#################### CONSTRUCTORS ####################
public:
	SegmentVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings)
	:	m_volumeSize(volumeSize), m_windowSettings(windowSettings)
	{}

	//#################### DESTRUCTOR ####################
protected:
	~SegmentVolumeDialog()
	{}

	//#################### PRIVATE ABSTRACT METHODS ####################
private:
	virtual wxPanel *create_modality_page(wxWindow *parent) = 0;

	//#################### PUBLIC METHODS ####################
public:
	const boost::optional<SegmentationOptions>& segmentation_options() const
	{
		return m_segmentationOptions;
	}

	//#################### PROTECTED METHODS ####################
protected:
	void initialise(wxWindow *parent)
	{
		// Note:	This code can't be put in SegmentVolumeDialog's constructor because it calls a virtual method (create_modality_page).
		//			It must therefore be placed here, and derived classes must call initialise(parent); in their constructors.

		Create(parent, wxID_ANY, wxT("Segment Volume"), wxDefaultPosition, wxDefaultSize);

		wxBookCtrlBase *notebook = GetBookCtrl();
		wxPanel *basicPage = create_basic_page(notebook);
		wxPanel *advancedPage = create_advanced_page(notebook);
		wxPanel *modalityPage = create_modality_page(notebook);
		notebook->AddPage(basicPage, wxT("Basic"), true);
		notebook->AddPage(advancedPage, wxT("Advanced"), false);
		notebook->AddPage(modalityPage, wxT("Modality-Specific"), false);

		CreateButtons();
		LayoutDialog();
	}

	//#################### PRIVATE METHODS ####################
private:
	wxPanel *create_advanced_page(wxWindow *parent)
	{
		wxPanel *panel = new wxPanel(parent);

		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		panel->SetSizer(sizer);

		// Set up the spin control to select the number of anisotropic diffusion filtering iterations.
		wxPanel *filteringPanel = new wxPanel(panel);
		sizer->Add(filteringPanel);
		wxGridSizer *filteringSizer = new wxGridSizer(0, 2, 0, 0);
		filteringPanel->SetSizer(filteringSizer);

		filteringSizer->Add(new wxStaticText(filteringPanel, wxID_ANY, wxT("ADF Iterations:")));
		m_adfIterations = new wxSpinCtrl(filteringPanel, wxID_ANY, wxT("20"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 30, 20);
		filteringSizer->Add(m_adfIterations);

		// Set up the spin control to select a waterfall layer limit.
		wxPanel *waterfallPanel = new wxPanel(panel);
		sizer->Add(waterfallPanel);
		wxGridSizer *waterfallSizer = new wxGridSizer(0, 2, 0, 0);
		waterfallPanel->SetSizer(waterfallSizer);

		waterfallSizer->Add(new wxStaticText(waterfallPanel, wxID_ANY, wxT("Waterfall Layer Limit:")));
		m_waterfallLayerLimit = new wxSpinCtrl(waterfallPanel, wxID_ANY, wxT("5"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 10, 5);
		waterfallSizer->Add(m_waterfallLayerLimit);

		sizer->Fit(panel);
		return panel;
	}

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
		sizer->Add(m_segmentationType, 0, wxALIGN_CENTER_HORIZONTAL);

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
			subvolumeSizer->Add(new wxStaticText(subvolumePanel, wxID_ANY, captions[i]));
			int initial = i != 2 ? m_volumeSize[i] : 1;
			wxString initialValue = string_to_wxString(boost::lexical_cast<std::string>(initial));
			m_subvolumeSizes[i] = new wxSpinCtrl(subvolumePanel, SPINID_GRIDSIZE, initialValue, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 1, m_volumeSize[i], initial);
			subvolumeSizer->Add(m_subvolumeSizes[i]);
		}

		sizer->Fit(panel);
		return panel;
	}
};

}

#endif
