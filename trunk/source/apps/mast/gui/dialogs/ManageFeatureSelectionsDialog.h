/***
 * millipede: ManageFeatureSelectionsDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MANAGEFEATURESELECTIONSDIALOG
#define H_MILLIPEDE_MANAGEFEATURESELECTIONSDIALOG

#include <boost/algorithm/string/trim.hpp>

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textdlg.h>

#include <common/partitionforests/base/PartitionForestMFSManager.h>
#include <mast/util/StringConversion.h>

namespace mp {

namespace mp_ManageFeatureSelectionsDialog {

//#################### ENUMERATIONS ####################
enum
{
	BUTTONID_CREATE_NEW,
};

template <typename MFS, typename Forest>
class ManageFeatureSelectionsDialog : public wxDialog
{
	//#################### TYPEDEFS ####################
private:
	typedef boost::shared_ptr<Forest> Forest_Ptr;
	typedef boost::shared_ptr<MFS> MFS_Ptr;
	typedef PartitionForestMFSManager<MFS> MFSManager;
	typedef boost::shared_ptr<MFSManager> MFSManager_Ptr;

	//#################### LISTENERS ####################
private:
	struct MFSManagerListener : MFSManager::Listener
	{
		ManageFeatureSelectionsDialog *base;

		explicit MFSManagerListener(ManageFeatureSelectionsDialog *base_)
		:	base(base_)
		{}

		void multi_feature_selection_manager_changed()
		{
			base->repopulate_mfs_choices();
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	Forest_Ptr m_forest;
	std::vector<wxChoice*> m_mfsChoices;
	MFSManager_Ptr m_mfsManager;
	boost::shared_ptr<MFSManagerListener> m_mfsManagerListener;

	//#################### CONSTRUCTORS ####################
public:
	ManageFeatureSelectionsDialog(wxWindow *parent, const MFSManager_Ptr& mfsManager, const Forest_Ptr& forest)
	:	wxDialog(parent, wxID_ANY, wxT("Manage Feature Selections"), wxDefaultPosition, wxDefaultSize),
		m_forest(forest),
		m_mfsManager(mfsManager),
		m_mfsManagerListener(new MFSManagerListener(this))
	{
		m_mfsManager->add_weak_listener(m_mfsManagerListener);
		setup_gui();
	}

	//#################### PRIVATE METHODS ####################
private:
	void add_mfs_choice(wxWindow *parent, wxSizer *parentSizer)
	{
		m_mfsChoices.push_back(new wxChoice(parent, wxID_ANY, wxDefaultPosition, wxSize(150,25), wxArrayString()));
		parentSizer->Add(m_mfsChoices.back());
	}

	std::string get_appropriate_name(const std::string& caption, const std::string& initialName)
	{
		std::string name = initialName;
		for(;;)
		{
			name = wxString_to_string(wxGetTextFromUser(wxT("Name:"), string_to_wxString(caption), string_to_wxString(name), this));
			boost::trim(name);

			if(name == "" || !m_mfsManager->has_multi_feature_selection(name)) break;

			// If the name wasn't appropriate, display an error message.
			wxMessageBox(wxT("The specified name is already in use."), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
		}
		return name;
	}

	void repopulate_mfs_choices()
	{
		wxArrayString mfsStrings;

		typedef typename MFSManager::MFSMap MFSMap;
		for(typename MFSMap::const_iterator it=m_mfsManager->multi_feature_selections().begin(), iend=m_mfsManager->multi_feature_selections().end(); it!=iend; ++it)
		{
			mfsStrings.Add(string_to_wxString(it->first));
		}

		for(std::vector<wxChoice*>::const_iterator it=m_mfsChoices.begin(), iend=m_mfsChoices.end(); it!=iend; ++it)
		{
			(*it)->Clear();
			for(size_t j=0, count=mfsStrings.GetCount(); j<count; ++j)
			{
				(*it)->Append(mfsStrings[j]);
			}
		}
	}

	void setup_gui()
	{
		wxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		SetSizer(sizer);

		wxPanel *inner = new wxPanel(this);
		wxFlexGridSizer *innerSizer = new wxFlexGridSizer(0, 1, 20, 0);
		inner->SetSizer(innerSizer);
		sizer->Add(inner, 0, wxALL, 10);

		// Nullary operations
		wxStaticBoxSizer *nullaryOperations = new wxStaticBoxSizer(wxVERTICAL, inner, wxT("Nullary Operations"));
		innerSizer->Add(nullaryOperations, 0, wxALIGN_CENTRE_HORIZONTAL);

		wxButton *createButton = new wxButton(inner, BUTTONID_CREATE_NEW, wxT("Create &New..."));
		nullaryOperations->Add(createButton, 0, wxALIGN_CENTRE_HORIZONTAL);

		// Unary operations
		wxStaticBoxSizer *unaryOperations = new wxStaticBoxSizer(wxVERTICAL, inner, wxT("Unary Operations"));
		innerSizer->Add(unaryOperations, 0, wxALIGN_CENTRE_HORIZONTAL);

		wxPanel *unaryInputs = new wxPanel(inner);
		wxFlexGridSizer *unaryInputsSizer = new wxFlexGridSizer(1, 0, 0, 5);
		unaryInputs->SetSizer(unaryInputsSizer);
			unaryInputsSizer->Add(new wxStaticText(unaryInputs, wxID_ANY, wxT("Input:")), 0, wxALIGN_CENTRE_VERTICAL);
			add_mfs_choice(unaryInputs, unaryInputsSizer);
		unaryOperations->Add(unaryInputs, 0, wxALIGN_CENTRE_HORIZONTAL);

		unaryOperations->AddSpacer(10);

		wxPanel *unaryButtons = new wxPanel(inner);
		wxFlexGridSizer *unaryButtonsSizer = new wxFlexGridSizer(1, 0, 0, 5);
		unaryButtons->SetSizer(unaryButtonsSizer);
			unaryButtonsSizer->Add(new wxButton(unaryButtons, wxID_ANY, wxT("&Clone...")));
			unaryButtonsSizer->Add(new wxButton(unaryButtons, wxID_ANY, wxT("&Delete")));
			unaryButtonsSizer->Add(new wxButton(unaryButtons, wxID_ANY, wxT("&Rename...")));
		unaryOperations->Add(unaryButtons, 0, wxALIGN_CENTRE_HORIZONTAL);

		// Binary operations
		wxStaticBoxSizer *binaryOperations = new wxStaticBoxSizer(wxVERTICAL, inner, wxT("Binary Operations"));
		innerSizer->Add(binaryOperations, 0, wxALIGN_CENTRE_HORIZONTAL);

		wxPanel *binaryInputs = new wxPanel(inner);
		wxFlexGridSizer *binaryInputsSizer = new wxFlexGridSizer(2, 0, 0, 5);
		binaryInputs->SetSizer(binaryInputsSizer);
			binaryInputsSizer->Add(new wxStaticText(binaryInputs, wxID_ANY, wxT("Left-Hand Input:")), 0, wxALIGN_CENTRE_VERTICAL);
			add_mfs_choice(binaryInputs, binaryInputsSizer);
			binaryInputsSizer->Add(new wxStaticText(binaryInputs, wxID_ANY, wxT("Right-Hand Input:")), 0, wxALIGN_CENTRE_VERTICAL);
			add_mfs_choice(binaryInputs, binaryInputsSizer);
		binaryOperations->Add(binaryInputs, 0, wxALIGN_CENTRE_HORIZONTAL);

		binaryOperations->AddSpacer(10);

		wxPanel *binaryButtons = new wxPanel(inner);
		wxFlexGridSizer *binaryButtonsSizer = new wxFlexGridSizer(1, 0, 0, 5);
		binaryButtons->SetSizer(binaryButtonsSizer);
			binaryButtonsSizer->Add(new wxButton(binaryButtons, wxID_ANY, wxT("&Intersect...")));
			binaryButtonsSizer->Add(new wxButton(binaryButtons, wxID_ANY, wxT("&Subtract...")));
			binaryButtonsSizer->Add(new wxButton(binaryButtons, wxID_ANY, wxT("&Union...")));
		binaryOperations->Add(binaryButtons, 0, wxALIGN_CENTRE_HORIZONTAL);

		// Close button
		innerSizer->Add(new wxButton(inner, wxID_CANCEL, wxT("Close")), 0, wxALIGN_CENTRE_HORIZONTAL);

		repopulate_mfs_choices();
		sizer->Fit(this);
		CentreOnParent();
	}

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonCreateNew(wxCommandEvent&)
	{
		std::string mfsName = get_appropriate_name("Create New Feature Selection", "Feature Selection");
		if(mfsName != "")
		{
			MFS_Ptr mfs(new MFS(m_forest));
			m_mfsManager->add_multi_feature_selection(mfsName, mfs);
			m_mfsManager->set_active_multi_feature_selection(mfsName);
		}
	}

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE_TEMPLATE2(ManageFeatureSelectionsDialog, wxDialog, MFS, Forest)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_CREATE_NEW, (ManageFeatureSelectionsDialog<MFS,Forest>::OnButtonCreateNew))
END_EVENT_TABLE()

}

using mp_ManageFeatureSelectionsDialog::ManageFeatureSelectionsDialog;

}

#endif
