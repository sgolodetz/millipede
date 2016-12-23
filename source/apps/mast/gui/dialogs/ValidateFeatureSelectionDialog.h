/***
 * millipede: ValidateFeatureSelectionDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VALIDATEFEATURESELECTIONDIALOG
#define H_MILLIPEDE_VALIDATEFEATURESELECTIONDIALOG

#include <sstream>

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/listctrl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include <common/io/files/DataTableFile.h>
#include <common/partitionforests/base/PartitionForestMFSManager.h>
#include <common/util/DataTable.h>
#include <mast/util/StringConversion.h>
#include "DialogUtil.h"

namespace mp {

template <typename MFS, typename Forest>
class ValidateFeatureSelectionDialog : public wxDialog
{
	//#################### ENUMERATIONS ####################
private:
	enum
	{
		BUTTONID_SAVE_TABLE
	};

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
			bottomSizer->Add(new wxButton(this, BUTTONID_SAVE_TABLE, wxT("&Save Table...")));
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
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonSaveTable(wxCommandEvent&)
	{
		wxFileDialog_Ptr dialog = construct_save_dialog(this, "Save Validation Table", "Comma-Separated Table (*.csv)|*.csv|LaTeX Table (*.tex)|*.tex");
		if(dialog->ShowModal() == wxID_OK)
		{
			// Construct the validation table.
			int rows = m_table->GetItemCount(), cols = m_table->GetColumnCount();
			DataTable table(rows+1, cols);

			// Fill in the header row.
			if(dialog->GetFilterIndex() == 0)
			{
				// Use the column headers from the table itself for CSV.
				for(int j=0; j<cols; ++j)
				{
					wxListItem item;
					item.SetMask(wxLIST_MASK_TEXT);
					m_table->GetColumn(j, item);
					table(0,j) = wxString_to_string(item.GetText());
				}
			}
			else
			{
				// Manually define headers that are more likely to fit on a page for LaTeX.
				table(0,0) = "Feature";
				table(0,1) = "Target";
				table(0,2) = "Gold";
				table(0,3) = "T - G";
				table(0,4) = "G - T";
				table(0,5) = "T $\\cap$ G";
				table(0,6) = "Dice";
				table(0,7) = "Jaccard";
			}

			// Fill in the rest of the table.
			for(int i=0; i<rows; ++i)
			{
				for(int j=0; j<cols; ++j)
				{
					// Get the text of the relevant cell.
					wxListItem item;
					item.SetId(i);
					item.SetColumn(j);
					item.SetMask(wxLIST_MASK_TEXT);
					m_table->GetItem(item);

					// Store it in the DataTable.
					table(i+1,j) = wxString_to_string(item.GetText());
				}
			}

			// Save it to the specified file.
			std::string path = wxString_to_string(dialog->GetPath());

			if(dialog->GetFilterIndex() == 0)
			{
				// Save the validation table as a comma-separated table.
				DataTableFile::save_csv(path, table);
			}
			else
			{
				// Save the validation table as a LaTeX table with a label row.
				DataTableFile::save_latex(path, table, true, "\\scriptsize");
			}
		}
	}

	//~~~~~~~~~~~~~~~~~~~~ CHOICES ~~~~~~~~~~~~~~~~~~~~
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

			m_table->InsertItem(f, string_to_wxString(feature_to_name(f)));
			m_table->SetItem(f, 1, string_to_wxString(boost::lexical_cast<std::string>(voxelsT)));
			m_table->SetItem(f, 2, string_to_wxString(boost::lexical_cast<std::string>(voxelsGS)));
			m_table->SetItem(f, 3, string_to_wxString(boost::lexical_cast<std::string>(voxelsTsubGS)));
			m_table->SetItem(f, 4, string_to_wxString(boost::lexical_cast<std::string>(voxelsGSsubT)));
			m_table->SetItem(f, 5, string_to_wxString(boost::lexical_cast<std::string>(voxelsTintersectGS)));

			if(voxelsT + voxelsGS != 0)
			{
				double diceSimilarityCoefficient = 2.0*voxelsTintersectGS / (voxelsT + voxelsGS);
				std::ostringstream oss;
				oss.setf(std::ios::fixed, std::ios::floatfield);
				oss.precision(3);
				oss << diceSimilarityCoefficient;
				m_table->SetItem(f, 6, string_to_wxString(oss.str()));
			}
			else
			{
				m_table->SetItem(f, 6, wxT("-"));
			}

			if(voxelsTunionGS != 0)
			{
				double jaccardSimilarityCoefficient = static_cast<double>(voxelsTintersectGS) / voxelsTunionGS;
				std::ostringstream oss;
				oss.setf(std::ios::fixed, std::ios::floatfield);
				oss.precision(3);
				oss << jaccardSimilarityCoefficient;
				m_table->SetItem(f, 7, string_to_wxString(oss.str()));
			}
			else
			{
				m_table->SetItem(f, 7, wxT("-"));
			}
		}

		fit_table();
	}

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateButtonSaveTable(wxUpdateUIEvent& e)
	{
		e.Enable(get_mfs_choice("Target") != "" && get_mfs_choice("Gold Standard") != "");
	}

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE_TEMPLATE2(ValidateFeatureSelectionDialog, wxDialog, MFS, Forest)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_SAVE_TABLE, (ValidateFeatureSelectionDialog<MFS,Forest>::OnButtonSaveTable))

	//~~~~~~~~~~~~~~~~~~~~ CHOICES ~~~~~~~~~~~~~~~~~~~~
	EVT_CHOICE(wxID_ANY, (ValidateFeatureSelectionDialog<MFS,Forest>::OnChoiceMFS))

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(BUTTONID_SAVE_TABLE, (ValidateFeatureSelectionDialog<MFS,Forest>::OnUpdateButtonSaveTable))
END_EVENT_TABLE()

}

#endif
