/***
 * millipede: ValidateFeatureSelectionDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VALIDATEFEATURESELECTIONDIALOG
#define H_MILLIPEDE_VALIDATEFEATURESELECTIONDIALOG

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/listctrl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include <common/partitionforests/base/PartitionForestMFSManager.h>
#include <mast/util/StringConversion.h>

namespace mp {

template <typename MFS, typename Forest>
class ValidateFeatureSelectionDialog : public wxDialog
{
	//#################### TYPEDEFS ####################
private:
	typedef typename MFS::Feature Feature;
	typedef boost::shared_ptr<Forest> Forest_Ptr;
	typedef boost::shared_ptr<MFS> MFS_Ptr;
	typedef boost::shared_ptr<const MFS> MFS_CPtr;
	typedef PartitionForestMFSManager<MFS> MFSManager;
	typedef boost::shared_ptr<const MFSManager> MFSManager_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	Forest_Ptr m_forest;
	std::map<std::string,wxChoice*> m_mfsChoices;
	MFSManager_CPtr m_mfsManager;
	wxListCtrl *m_table;

	//#################### CONSTRUCTORS ####################
public:
	ValidateFeatureSelectionDialog(wxWindow *parent, const MFSManager_CPtr& mfsManager, const Forest_Ptr& forest)
	:	wxDialog(parent, wxID_ANY, wxT("Validate Feature Selection"), wxDefaultPosition, wxDefaultSize),
		m_forest(forest),
		m_mfsManager(mfsManager)
	{
		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		SetSizer(sizer);

		wxFlexGridSizer *innerSizer = new wxFlexGridSizer(0, 1, 20, 0);
		sizer->Add(innerSizer, 0, wxALL, 10);

		wxFlexGridSizer *topSizer = new wxFlexGridSizer(1, 0, 0, 5);
			topSizer->Add(new wxStaticText(this, wxID_ANY, wxT("Target:")), 0, wxALIGN_CENTRE_VERTICAL);
			add_mfs_choice("Target", this, topSizer);
			topSizer->AddSpacer(10);
			topSizer->Add(new wxStaticText(this, wxID_ANY, wxT("Gold Standard:")), 0, wxALIGN_CENTRE_VERTICAL);
			add_mfs_choice("Gold Standard", this, topSizer);
		innerSizer->Add(topSizer, 0, wxALIGN_CENTRE_HORIZONTAL);

		m_table = new wxListCtrl(this, wxID_ANY, wxDefaultPosition, wxSize(1000,300), wxLC_REPORT|wxLC_SINGLE_SEL|wxLC_HRULES|wxLC_VRULES);
		m_table->InsertColumn(0, wxT("Feature"));
		m_table->InsertColumn(1, wxT("Target Voxels"));
		m_table->InsertColumn(2, wxT("Gold Standard Voxels"));
		m_table->InsertColumn(3, wxT("Extraneous Voxels (T - GS)"));
		m_table->InsertColumn(4, wxT("Missing Voxels (GS - T)"));
		m_table->InsertColumn(5, wxT("Overlapping Voxels"));
		m_table->InsertColumn(6, wxT("Dice Similarity Coefficient"));
		m_table->InsertColumn(7, wxT("Jaccard Similarity Coefficient"));
		innerSizer->Add(m_table, 0, wxALIGN_CENTRE_HORIZONTAL);

		fit_table();

		wxFlexGridSizer *bottomSizer = new wxFlexGridSizer(1, 0, 0, 5);
			bottomSizer->Add(new wxButton(this, wxID_ANY, wxT("&Save Table...")));
			bottomSizer->Add(new wxButton(this, wxID_CANCEL, wxT("&Close")));
		innerSizer->Add(bottomSizer, 0, wxALIGN_CENTRE_HORIZONTAL);

		populate_mfs_choices();
		sizer->Fit(this);
		CentreOnParent();
	}

	//#################### PRIVATE METHODS ####################
private:
	void add_mfs_choice(const std::string& choiceName, wxWindow *parent, wxSizer *sizer)
	{
		// FIXME: This is the same as ManageFeatureSelectionsDialog::add_mfs_choice() - the commonality should be factored out.
		wxChoice *choice = new wxChoice(parent, wxID_ANY, wxDefaultPosition, wxSize(150,25), wxArrayString());
		m_mfsChoices.insert(std::make_pair(choiceName, choice));
		sizer->Add(choice);
	}

	void fit_table()
	{
		for(int i=0, count=m_table->GetColumnCount(); i<count; ++i)
		{
			m_table->SetColumnWidth(i, wxLIST_AUTOSIZE_USEHEADER);
		}
	}

	std::string get_mfs_choice(const std::string& choiceName) const
	{
		// FIXME: This is the same as ManageFeatureSelectionsDialog::get_mfs_choice() - the commonality should be factored out.
		return wxString_to_string(m_mfsChoices.find(choiceName)->second->GetStringSelection());
	}

	void populate_mfs_choices()
	{
		// FIXME: This is quite similar to ManageFeatureSelectionsDialog::repopulate_mfs_lists() - the commonality should be factored out.
		wxArrayString mfsStrings;

		typedef typename MFSManager::MFSMap MFSMap;
		for(typename MFSMap::const_iterator it=m_mfsManager->multi_feature_selections().begin(), iend=m_mfsManager->multi_feature_selections().end(); it!=iend; ++it)
		{
			mfsStrings.Add(string_to_wxString(it->first));
		}

		for(std::map<std::string,wxChoice*>::const_iterator it=m_mfsChoices.begin(), iend=m_mfsChoices.end(); it!=iend; ++it)
		{
			wxChoice *choice = it->second;
			for(size_t j=0, count=mfsStrings.GetCount(); j<count; ++j)
			{
				choice->Append(mfsStrings[j]);
			}
		}
	}

	//#################### EVENT HANDLERS ####################
public:
	void OnChoiceMFS(wxCommandEvent&)
	{
		std::string targetName = get_mfs_choice("Target");
		std::string goldStandardName = get_mfs_choice("Gold Standard");
		if(targetName == "" || goldStandardName == "") return;

		MFS_CPtr T = m_mfsManager->multi_feature_selection(targetName);
		MFS_CPtr GS = m_mfsManager->multi_feature_selection(goldStandardName);

		// Calculate relevant derived multi-feature selections.
		MFS_Ptr TsubGS(new MFS(m_forest));			TsubGS->subtract(T, GS);
		MFS_Ptr GSsubT(new MFS(m_forest));			GSsubT->subtract(GS, T);
		MFS_Ptr TunionGS(new MFS(m_forest));		TunionGS->combine(T, GS);
		MFS_Ptr TsymdiffGS(new MFS(m_forest));		TsymdiffGS->combine(TsubGS, GSsubT);
		MFS_Ptr TintersectGS(new MFS(m_forest));	TintersectGS->subtract(TunionGS, TsymdiffGS);

		m_table->DeleteAllItems();
		for(Feature f=enum_begin<Feature>(), end=enum_end<Feature>(); f!=end; ++f)
		{
			int	voxelsT = T->voxel_count(f),
				voxelsGS = GS->voxel_count(f),
				voxelsTsubGS = TsubGS->voxel_count(f),
				voxelsGSsubT = GSsubT->voxel_count(f),
				voxelsTunionGS = TunionGS->voxel_count(f),
				voxelsTintersectGS = TintersectGS->voxel_count(f);

			m_table->InsertItem(f, string_to_wxString(feature_name(f)));
			m_table->SetItem(f, 1, string_to_wxString(boost::lexical_cast<std::string>(voxelsT)));
			m_table->SetItem(f, 2, string_to_wxString(boost::lexical_cast<std::string>(voxelsGS)));
			m_table->SetItem(f, 3, string_to_wxString(boost::lexical_cast<std::string>(voxelsTsubGS)));
			m_table->SetItem(f, 4, string_to_wxString(boost::lexical_cast<std::string>(voxelsGSsubT)));
			m_table->SetItem(f, 5, string_to_wxString(boost::lexical_cast<std::string>(voxelsTintersectGS)));

			if(voxelsT + voxelsGS != 0)
			{
				m_table->SetItem(f, 6, string_to_wxString(boost::lexical_cast<std::string>(2.0*voxelsTintersectGS / (voxelsT + voxelsGS))));
			}
			else
			{
				m_table->SetItem(f, 6, wxT("-"));
			}

			if(voxelsTunionGS != 0)
			{
				m_table->SetItem(f, 7, string_to_wxString(boost::lexical_cast<std::string>(static_cast<double>(voxelsTintersectGS) / voxelsTunionGS)));
			}
			else
			{
				m_table->SetItem(f, 7, wxT("-"));
			}
		}

		fit_table();
	}

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE_TEMPLATE2(ValidateFeatureSelectionDialog, wxDialog, MFS, Forest)
	EVT_CHOICE(wxID_ANY, (ValidateFeatureSelectionDialog<MFS,Forest>::OnChoiceMFS))
END_EVENT_TABLE()

}

#endif
