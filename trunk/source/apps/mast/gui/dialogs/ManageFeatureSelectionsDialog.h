/***
 * millipede: ManageFeatureSelectionsDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MANAGEFEATURESELECTIONSDIALOG
#define H_MILLIPEDE_MANAGEFEATURESELECTIONSDIALOG

#include <cassert>

#include <boost/algorithm/string/trim.hpp>

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/listctrl.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textdlg.h>

#include <common/io/util/OSSWrapper.h>
#include <common/partitionforests/base/PartitionForestMFSManager.h>
#include <mast/util/StringConversion.h>

namespace mp {

namespace mp_ManageFeatureSelectionsDialog {

//#################### ENUMERATIONS ####################
enum
{
	BUTTONID_CLONE,
	BUTTONID_CREATE_NEW,
	BUTTONID_DELETE,
	BUTTONID_INTERSECT,
	BUTTONID_RENAME,
	BUTTONID_SUBTRACT,
	BUTTONID_SYMDIFF,
	BUTTONID_UNION,
};

template <typename MFS, typename Forest>
class ManageFeatureSelectionsDialog : public wxDialog
{
	//#################### TYPEDEFS ####################
private:
	typedef boost::shared_ptr<Forest> Forest_Ptr;
	typedef boost::shared_ptr<MFS> MFS_Ptr;
	typedef boost::shared_ptr<const MFS> MFS_CPtr;
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
			base->repopulate_mfs_lists();
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	Forest_Ptr m_forest;
	std::map<std::string,wxChoice*> m_mfsChoices;
	wxListCtrl *m_mfsList;
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
	void add_mfs_choice(const std::string& choiceName, wxWindow *parent, wxSizer *sizer)
	{
		wxChoice *choice = new wxChoice(parent, wxID_ANY, wxDefaultPosition, wxSize(150,25), wxArrayString());
		m_mfsChoices.insert(std::make_pair(choiceName, choice));
		sizer->Add(choice);
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

	std::string get_mfs_choice(const std::string& choiceName) const
	{
		return wxString_to_string(m_mfsChoices.find(choiceName)->second->GetStringSelection());
	}

	void repopulate_mfs_lists()
	{
		wxArrayString mfsStrings;

		typedef typename MFSManager::MFSMap MFSMap;
		for(typename MFSMap::const_iterator it=m_mfsManager->multi_feature_selections().begin(), iend=m_mfsManager->multi_feature_selections().end(); it!=iend; ++it)
		{
			mfsStrings.Add(string_to_wxString(it->first));
		}

		m_mfsList->DeleteAllItems();
		for(size_t i=0, count=mfsStrings.GetCount(); i<count; ++i)
		{
			m_mfsList->InsertItem(i, mfsStrings[i]);
		}

		for(std::map<std::string,wxChoice*>::const_iterator it=m_mfsChoices.begin(), iend=m_mfsChoices.end(); it!=iend; ++it)
		{
			wxChoice *choice = it->second;
			choice->Clear();
			for(size_t j=0, count=mfsStrings.GetCount(); j<count; ++j)
			{
				choice->Append(mfsStrings[j]);
			}

			assert(choice->GetCount() != 0);
			choice->SetSelection(0);
		}
	}

	void setup_gui()
	{
		wxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		SetSizer(sizer);

		wxPanel *inner = new wxPanel(this);
		wxFlexGridSizer *innerSizer = new wxFlexGridSizer(1, 0, 0, 20);
		inner->SetSizer(innerSizer);
		sizer->Add(inner, 0, wxALL, 10);

		//~~~~~~~~~~~~~~~~~
		// Operations Panel
		//~~~~~~~~~~~~~~~~~

		wxPanel *operations = new wxPanel(inner);
		wxFlexGridSizer *operationsSizer = new wxFlexGridSizer(0, 1, 20, 0);
		operations->SetSizer(operationsSizer);
		innerSizer->Add(operations);

		// Nullary operations
		wxStaticBoxSizer *nullaryOperations = new wxStaticBoxSizer(wxVERTICAL, operations, wxT("Nullary Operations"));
		operationsSizer->Add(nullaryOperations, 0, wxALIGN_CENTRE_HORIZONTAL);

		wxButton *createButton = new wxButton(operations, BUTTONID_CREATE_NEW, wxT("Create &New..."));
		nullaryOperations->Add(createButton, 0, wxALIGN_CENTRE_HORIZONTAL|wxALL, 5);

		// Unary operations
		wxStaticBoxSizer *unaryOperations = new wxStaticBoxSizer(wxVERTICAL, operations, wxT("Unary Operations"));
		operationsSizer->Add(unaryOperations, 0, wxALIGN_CENTRE_HORIZONTAL);

		wxPanel *unaryInputs = new wxPanel(operations);
		wxFlexGridSizer *unaryInputsSizer = new wxFlexGridSizer(1, 0, 0, 5);
		unaryInputs->SetSizer(unaryInputsSizer);
			unaryInputsSizer->Add(new wxStaticText(unaryInputs, wxID_ANY, wxT("Input:")), 0, wxALIGN_CENTRE_VERTICAL);
			add_mfs_choice("UInput", unaryInputs, unaryInputsSizer);
		unaryOperations->Add(unaryInputs, 0, wxALIGN_CENTRE_HORIZONTAL);

		unaryOperations->AddSpacer(10);

		wxPanel *unaryButtons = new wxPanel(operations);
		wxFlexGridSizer *unaryButtonsSizer = new wxFlexGridSizer(1, 0, 0, 5);
		unaryButtons->SetSizer(unaryButtonsSizer);
			unaryButtonsSizer->Add(new wxButton(unaryButtons, BUTTONID_CLONE, wxT("&Clone...")), 0, wxALL, 5);
			unaryButtonsSizer->Add(new wxButton(unaryButtons, BUTTONID_DELETE, wxT("&Delete")), 0, wxALL, 5);
			unaryButtonsSizer->Add(new wxButton(unaryButtons, BUTTONID_RENAME, wxT("&Rename...")), 0, wxALL, 5);
		unaryOperations->Add(unaryButtons, 0, wxALIGN_CENTRE_HORIZONTAL);

		// Binary operations
		wxStaticBoxSizer *binaryOperations = new wxStaticBoxSizer(wxVERTICAL, operations, wxT("Binary Operations"));
		operationsSizer->Add(binaryOperations, 0, wxALIGN_CENTRE_HORIZONTAL);

		wxPanel *binaryInputs = new wxPanel(operations);
		wxFlexGridSizer *binaryInputsSizer = new wxFlexGridSizer(2, 0, 0, 5);
		binaryInputs->SetSizer(binaryInputsSizer);
			binaryInputsSizer->Add(new wxStaticText(binaryInputs, wxID_ANY, wxT("Left-Hand Input:")), 0, wxALIGN_CENTRE_VERTICAL);
			add_mfs_choice("BLInput", binaryInputs, binaryInputsSizer);
			binaryInputsSizer->Add(new wxStaticText(binaryInputs, wxID_ANY, wxT("Right-Hand Input:")), 0, wxALIGN_CENTRE_VERTICAL);
			add_mfs_choice("BRInput", binaryInputs, binaryInputsSizer);
		binaryOperations->Add(binaryInputs, 0, wxALIGN_CENTRE_HORIZONTAL);

		binaryOperations->AddSpacer(10);

		wxPanel *binaryButtons = new wxPanel(operations);
		wxFlexGridSizer *binaryButtonsSizer = new wxFlexGridSizer(2, 2, 0, 0);
		binaryButtons->SetSizer(binaryButtonsSizer);
			binaryButtonsSizer->Add(new wxButton(binaryButtons, BUTTONID_INTERSECT, wxT("&Intersect...")), 0, wxALIGN_CENTRE_HORIZONTAL|wxALL, 5);
			binaryButtonsSizer->Add(new wxButton(binaryButtons, BUTTONID_UNION, wxT("&Union...")), 0, wxALIGN_CENTRE_HORIZONTAL|wxALL, 5);
			binaryButtonsSizer->Add(new wxButton(binaryButtons, BUTTONID_SUBTRACT, wxT("&Subtract...")), 0, wxALIGN_CENTRE_HORIZONTAL|wxALL, 5);
			binaryButtonsSizer->Add(new wxButton(binaryButtons, BUTTONID_SYMDIFF, wxT("Sym&Diff...")), 0, wxALIGN_CENTRE_HORIZONTAL|wxALL, 5);
		binaryOperations->Add(binaryButtons, 0, wxALIGN_CENTRE_HORIZONTAL);

		// Close button
		operationsSizer->Add(new wxButton(operations, wxID_CANCEL, wxT("Close")), 0, wxALIGN_CENTRE_HORIZONTAL|wxALL, 5);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Feature Selections Display
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~

		m_mfsList = new wxListCtrl(inner, wxID_ANY, wxDefaultPosition, wxSize(175,300), wxLC_REPORT|wxLC_SINGLE_SEL);
		m_mfsList->InsertColumn(0, wxT("Feature Selections"));
		m_mfsList->SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER);
		innerSizer->Add(m_mfsList);

		repopulate_mfs_lists();
		sizer->Fit(this);
		CentreOnParent();
	}

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonClone(wxCommandEvent&)
	{
		std::string name = get_mfs_choice("UInput");
		std::string cloneName = get_appropriate_name("Clone Feature Selection", name);
		if(cloneName != "")
		{
			MFS_Ptr clone(new MFS(*m_mfsManager->multi_feature_selection(name)));
			m_mfsManager->add_multi_feature_selection(cloneName, clone);
			m_mfsManager->set_active_multi_feature_selection(cloneName);
		}
	}

	void OnButtonCreateNew(wxCommandEvent&)
	{
		std::string name = get_appropriate_name("Create New Feature Selection", "Feature Selection");
		if(name != "")
		{
			MFS_Ptr mfs(new MFS(m_forest));
			m_mfsManager->add_multi_feature_selection(name, mfs);
			m_mfsManager->set_active_multi_feature_selection(name);
		}
	}

	void OnButtonDelete(wxCommandEvent&)
	{
		std::string name = get_mfs_choice("UInput");
		m_mfsManager->remove_multi_feature_selection(name);
	}

	void OnButtonIntersect(wxCommandEvent&)
	{
		std::string lhsName = get_mfs_choice("BLInput"), rhsName = get_mfs_choice("BRInput");
		std::string combinedName = get_appropriate_name("Intersect Feature Selections", OSSWrapper() << "Intersect(" << lhsName << ',' << rhsName << ')');
		if(combinedName != "")
		{
			MFS_CPtr lhs = m_mfsManager->multi_feature_selection(lhsName);
			MFS_CPtr rhs = m_mfsManager->multi_feature_selection(rhsName);
			MFS_Ptr LunionR(new MFS(m_forest));		LunionR->combine(lhs, rhs);
			MFS_Ptr LsubR(new MFS(m_forest));		LsubR->subtract(lhs, rhs);
			MFS_Ptr RsubL(new MFS(m_forest));		RsubL->subtract(rhs, lhs);
			MFS_Ptr LsymdiffR(new MFS(m_forest));	LsymdiffR->combine(LsubR, RsubL);
			MFS_Ptr mfs(new MFS(m_forest));			mfs->subtract(LunionR, LsymdiffR);
			m_mfsManager->add_multi_feature_selection(combinedName, mfs);
			m_mfsManager->set_active_multi_feature_selection(combinedName);
		}
	}

	void OnButtonRename(wxCommandEvent&)
	{
		std::string oldName = get_mfs_choice("UInput");
		std::string newName = get_appropriate_name("Rename Feature Selection", oldName);
		if(newName != "")
		{
			m_mfsManager->rename_multi_feature_selection(oldName, newName);
		}
	}

	void OnButtonSubtract(wxCommandEvent&)
	{
		std::string lhsName = get_mfs_choice("BLInput"), rhsName = get_mfs_choice("BRInput");
		std::string combinedName = get_appropriate_name("Subtract Feature Selections", OSSWrapper() << "Subtract(" << lhsName << ',' << rhsName << ')');
		if(combinedName != "")
		{
			MFS_Ptr mfs(new MFS(m_forest));
			mfs->subtract(m_mfsManager->multi_feature_selection(lhsName), m_mfsManager->multi_feature_selection(rhsName));
			m_mfsManager->add_multi_feature_selection(combinedName, mfs);
			m_mfsManager->set_active_multi_feature_selection(combinedName);
		}
	}

	void OnButtonSymDiff(wxCommandEvent&)
	{
		std::string lhsName = get_mfs_choice("BLInput"), rhsName = get_mfs_choice("BRInput");
		std::string combinedName = get_appropriate_name("SymDiff Feature Selections", OSSWrapper() << "SymDiff(" << lhsName << ',' << rhsName << ')');
		if(combinedName != "")
		{
			MFS_CPtr lhs = m_mfsManager->multi_feature_selection(lhsName);
			MFS_CPtr rhs = m_mfsManager->multi_feature_selection(rhsName);
			MFS_Ptr LsubR(new MFS(m_forest));	LsubR->subtract(lhs, rhs);
			MFS_Ptr RsubL(new MFS(m_forest));	RsubL->subtract(rhs, lhs);
			MFS_Ptr mfs(new MFS(m_forest));		mfs->combine(LsubR, RsubL);
			m_mfsManager->add_multi_feature_selection(combinedName, mfs);
			m_mfsManager->set_active_multi_feature_selection(combinedName);
		}
	}

	void OnButtonUnion(wxCommandEvent&)
	{
		std::string lhsName = get_mfs_choice("BLInput"), rhsName = get_mfs_choice("BRInput");
		std::string combinedName = get_appropriate_name("Union Feature Selections", OSSWrapper() << "Union(" << lhsName << ',' << rhsName << ')');
		if(combinedName != "")
		{
			MFS_Ptr mfs(new MFS(m_forest));
			mfs->combine(m_mfsManager->multi_feature_selection(lhsName), m_mfsManager->multi_feature_selection(rhsName));
			m_mfsManager->add_multi_feature_selection(combinedName, mfs);
			m_mfsManager->set_active_multi_feature_selection(combinedName);
		}
	}

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateButtonDelete(wxUpdateUIEvent& e)
	{
		e.Enable(m_mfsManager->multi_feature_selections().size() > 1);
	}

	void OnUpdateButtonBinaryOp(wxUpdateUIEvent& e)
	{
		e.Enable(get_mfs_choice("BLInput") != get_mfs_choice("BRInput"));
	}

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE_TEMPLATE2(ManageFeatureSelectionsDialog, wxDialog, MFS, Forest)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_CLONE, (ManageFeatureSelectionsDialog<MFS,Forest>::OnButtonClone))
	EVT_BUTTON(BUTTONID_CREATE_NEW, (ManageFeatureSelectionsDialog<MFS,Forest>::OnButtonCreateNew))
	EVT_BUTTON(BUTTONID_DELETE, (ManageFeatureSelectionsDialog<MFS,Forest>::OnButtonDelete))
	EVT_BUTTON(BUTTONID_INTERSECT, (ManageFeatureSelectionsDialog<MFS,Forest>::OnButtonIntersect))
	EVT_BUTTON(BUTTONID_RENAME, (ManageFeatureSelectionsDialog<MFS,Forest>::OnButtonRename))
	EVT_BUTTON(BUTTONID_SUBTRACT, (ManageFeatureSelectionsDialog<MFS,Forest>::OnButtonSubtract))
	EVT_BUTTON(BUTTONID_SYMDIFF, (ManageFeatureSelectionsDialog<MFS,Forest>::OnButtonSymDiff))
	EVT_BUTTON(BUTTONID_UNION, (ManageFeatureSelectionsDialog<MFS,Forest>::OnButtonUnion))

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(BUTTONID_DELETE, (ManageFeatureSelectionsDialog<MFS,Forest>::OnUpdateButtonDelete))
	EVT_UPDATE_UI(BUTTONID_INTERSECT, (ManageFeatureSelectionsDialog<MFS,Forest>::OnUpdateButtonBinaryOp))
	EVT_UPDATE_UI(BUTTONID_SUBTRACT, (ManageFeatureSelectionsDialog<MFS,Forest>::OnUpdateButtonBinaryOp))
	EVT_UPDATE_UI(BUTTONID_SYMDIFF, (ManageFeatureSelectionsDialog<MFS,Forest>::OnUpdateButtonBinaryOp))
	EVT_UPDATE_UI(BUTTONID_UNION, (ManageFeatureSelectionsDialog<MFS,Forest>::OnUpdateButtonBinaryOp))
END_EVENT_TABLE()

}

using mp_ManageFeatureSelectionsDialog::ManageFeatureSelectionsDialog;

}

#endif
