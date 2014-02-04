/***
 * millipede: SegmentDICOMVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTMULTIPLEDIALOG
#define H_MILLIPEDE_SEGMENTMULTIPLEDIALOG

#include <wx/textctrl.h>

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <boost/optional.hpp>

#include <vector>

#include <common/segmentation/DICOMSegmentationOptions.h>
#include <mast/models/SegmentationRun.h>
#include <common/jobs/CompositeJob.h>



namespace mp {
	
namespace mp_segmentMultipleDialog {	
	
//#################### ENUMERATIONS ####################
enum
{
	BUTTONID_ADD,
	BUTTONID_OK,
	BUTTONID_CANCEL,
	BUTTONID_COLOUR_BASE,
	BUTTONID_COLOUR_LAST = (BUTTONID_COLOUR_BASE + 1) + 50,
};


template <typename LL, typename BL, typename F>
class SegmentMultipleDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:

	typedef SegmentationRun<LL, BL, F> SegRun;
	typedef std::vector<SegRun*> OptionList;
	typedef VolumeIPF<LL,BL> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef VolumeIPFBuilder<DICOMLowestLayersBuilder> DICOMVolumeIPFBuilder;
	
	OptionList* m_runs;
	
	wxWindow* m_parent;
	DICOMVolume_Ptr m_dicomVolume;
	
	WindowSettings* m_ws;
	wxBoxSizer* sizer;
	
	wxPanel* list;
	wxBoxSizer* listSizer;
	
	int n;

	//#################### CONSTRUCTORS ####################
public:
	SegmentMultipleDialog(wxWindow *parent, const itk::Size<3>& volumeSize, WindowSettings& windowSettings, DICOMVolume_Ptr dv):	wxDialog(parent, wxID_ANY, wxT("Choose segmentation methods"), wxDefaultPosition, wxDefaultSize)
	{
		
		m_runs = new OptionList();
		m_parent = parent;
		m_dicomVolume = dv;
		m_ws = &windowSettings;
		
		
		sizer = new wxBoxSizer(wxVERTICAL);
		
		SetSizer(sizer);
		
		//refresh();
		
		list = new wxPanel(this);
		listSizer = new wxBoxSizer(wxVERTICAL);
		
		list->SetSizer(listSizer);
		
		
		sizer->Add(list, 1, wxEXPAND);
		
		//sizer->AddSpacer(10);
		
		wxPanel *bottomPanel = new wxPanel(this);
		wxBoxSizer *bottom = new wxBoxSizer(wxHORIZONTAL);
		bottomPanel->SetSizer(bottom);
		bottom->Fit(bottomPanel);
		
		wxButton* addButton = new wxButton(bottomPanel, BUTTONID_ADD, wxT("Add new..."));
		wxButton* okButton = new wxButton(bottomPanel, BUTTONID_OK, wxT("OK"));
		wxButton* cancelButton = new wxButton(bottomPanel, BUTTONID_CANCEL, wxT("Cancel"));
		
		bottom->Add(addButton, 0, wxALIGN_CENTER_VERTICAL);
		bottom->Add(okButton, 0, wxALIGN_CENTER_VERTICAL);
		bottom->Add(cancelButton, 0, wxALIGN_CENTER_VERTICAL);
		
		sizer->Add(bottomPanel, 0, wxALIGN_CENTER_HORIZONTAL);
		
		//sizer->Fit(this);
		
		n = 0;
	
	}
	
	//#################### PUBLIC METHODS ###################
	OptionList* get_runs() {
		return m_runs;
	}

	//#################### PRIVATE METHODS ####################

	void refresh()
	{
		/*sizer = new wxBoxSizer(wxVERTICAL);
		
		SetSizer(sizer);*/
		
		std::vector<wxButton*> colourButtons;
		
		for(unsigned i = 0; i < m_runs->size(); i++)
		{
			wxPanel *entryPanel = new wxPanel(list);
			wxBoxSizer *entry = new wxBoxSizer(wxHORIZONTAL);
			
			entry->Add(new wxStaticText(entryPanel, -1, wxT("Method " + i)), 1, wxALIGN_CENTER_VERTICAL);
			
			wxButton *b = new wxButton(entryPanel, BUTTONID_COLOUR_BASE + i, wxT("Colour"));
			
			entry->Add(b, 0, wxALIGN_CENTER_VERTICAL);
			colourButtons.push_back(b);
			
			entryPanel->SetSizer(entry);
			
			listSizer->Add(entryPanel, 1, wxALIGN_CENTER_HORIZONTAL);
			
			entry->Fit(entryPanel);
		}
		
		/*sizer->AddSpacer(10);
		
		wxPanel *bottomPanel = new wxPanel(this);
		wxBoxSizer *bottom = new wxBoxSizer(wxHORIZONTAL);
		bottomPanel->SetSizer(bottom);
		bottom->Fit(bottomPanel);
		
		wxButton* addButton = new wxButton(bottomPanel, BUTTONID_ADD, wxT("Add new..."));
		wxButton* okButton = new wxButton(bottomPanel, BUTTONID_OK, wxT("OK"));
		wxButton* cancelButton = new wxButton(bottomPanel, BUTTONID_CANCEL, wxT("Cancel"));
		
		bottom->Add(addButton, 0, wxALIGN_CENTER_VERTICAL);
		bottom->Add(okButton, 0, wxALIGN_CENTER_VERTICAL);
		bottom->Add(cancelButton, 0, wxALIGN_CENTER_VERTICAL);
		
		sizer->Add(bottomPanel, 0, wxALIGN_CENTER_HORIZONTAL);
		
		sizer->Fit(this);*/
	}

	void OnOK(wxCommandEvent&) {
		Close();
	}
	
	void OnCancel(wxCommandEvent&) {
		
		Close();
	}
	
	void ColourPopup(wxCommandEvent&) {
		//wxColour c = wxColour::wxGetColourFromUser(this);
		//if (c.IsOk()) {
		//	
		//}
			
	}
	
	void addRun(wxCommandEvent&) {

		// Display a segment volume dialog to allow the user to choose how the segmentation process should work.
		
		const WindowSettings ws = *m_ws;
		
		SegmentDICOMVolumeDialog dialog(m_parent, m_dicomVolume->size(), ws);
		dialog.ShowModal();
		if(dialog.segmentation_options())
		{
			const boost::optional<DICOMSegmentationOptions> opOptions = dialog.segmentation_options();
			
			const DICOMSegmentationOptions options = *opOptions;
			
			SegRun *run = new SegmentationRun<LL,BL,F>(&options);
			
			/*wxColour colour = wxColour::wxGetColourFromUser(this);
			
			if (colour.IsOk()) {
				run->setColour(colour);
			}
			else {
				run->setColour(wxColour::WXBLUE);
			}*/
			
			run->getVolumeIPF();
			
			run->make_job(m_dicomVolume);
			
			m_runs->push_back(run);
			
			(*m_runs)[m_runs->size() - 1]->getVolumeIPF();
			
			std::cout << "m_runs size " << m_runs->size() << std::endl;
			
			n++;
			
			
			
			
			wxPanel *entryPanel = new wxPanel(this);
			wxBoxSizer *entry = new wxBoxSizer(wxHORIZONTAL);
			
			entry->Add(new wxStaticText(entryPanel, -1 ,wxString::Format(wxT("Method %i"),n)), 1, wxALIGN_CENTER_VERTICAL);
			
			//wxButton *b = new wxButton(entryPanel, BUTTONID_COLOUR_BASE + n, wxT("Colour"));
			
			//entry->Add(b, 1, wxALIGN_CENTER_VERTICAL);
			//colourButtons.push_back(b);
			
			entryPanel->SetSizer(entry);
			
			listSizer->Add(entryPanel, 1, wxEXPAND);
			
			entry->Fit(entryPanel);
			
			listSizer->Fit(list);
			
			//sizer->Fit(this);
		}
		
		//refresh();
		
	}
	
	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
	
};
//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE_TEMPLATE3(SegmentMultipleDialog, wxDialog, LL, BL, F)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_ADD, (SegmentMultipleDialog<LL, BL, F>::addRun))
	EVT_BUTTON(BUTTONID_OK, (SegmentMultipleDialog<LL, BL, F>::OnOK))
	EVT_BUTTON(BUTTONID_CANCEL, (SegmentMultipleDialog<LL, BL, F>::OnCancel))
END_EVENT_TABLE()

}

using mp_segmentMultipleDialog::SegmentMultipleDialog;

}

#endif
