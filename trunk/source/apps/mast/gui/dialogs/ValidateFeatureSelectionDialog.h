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

namespace mp {

// TODO: This will need to be templatised appropriately.
class ValidateFeatureSelectionDialog : public wxDialog
{
	//#################### CONSTRUCTORS ####################
public:
	explicit ValidateFeatureSelectionDialog(wxWindow *parent)
	:	wxDialog(parent, wxID_ANY, wxT("Validate Feature Selection"), wxDefaultPosition, wxDefaultSize)
	{
		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		SetSizer(sizer);

		wxFlexGridSizer *innerSizer = new wxFlexGridSizer(0, 1, 20, 0);
		sizer->Add(innerSizer, 0, wxALL, 10);

		wxFlexGridSizer *topSizer = new wxFlexGridSizer(1, 0, 0, 5);
			topSizer->Add(new wxStaticText(this, wxID_ANY, wxT("Target:")), 0, wxALIGN_CENTRE_VERTICAL);
			topSizer->Add(new wxChoice(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxArrayString()), 0, wxALIGN_CENTRE_VERTICAL);
			topSizer->AddSpacer(10);
			topSizer->Add(new wxStaticText(this, wxID_ANY, wxT("Gold Standard:")), 0, wxALIGN_CENTRE_VERTICAL);
			topSizer->Add(new wxChoice(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxArrayString()), 0, wxALIGN_CENTRE_VERTICAL);
		innerSizer->Add(topSizer, 0, wxALIGN_CENTRE_HORIZONTAL);

		wxListCtrl *comparisonList = new wxListCtrl(this, wxID_ANY, wxDefaultPosition, wxSize(1000,300), wxLC_REPORT|wxLC_SINGLE_SEL|wxLC_HRULES|wxLC_VRULES);
		comparisonList->InsertColumn(0, wxT("Feature"));
		comparisonList->InsertColumn(1, wxT("Target Voxels"));
		comparisonList->InsertColumn(2, wxT("Gold Standard Voxels"));
		comparisonList->InsertColumn(3, wxT("Extraneous Voxels (T - GS)"));
		comparisonList->InsertColumn(4, wxT("Missing Voxels (GS - T)"));
		comparisonList->InsertColumn(5, wxT("Overlapping Voxels"));
		comparisonList->InsertColumn(6, wxT("Dice Similarity Coefficient"));
		comparisonList->InsertColumn(7, wxT("Jaccard Similarity Coefficient"));
		innerSizer->Add(comparisonList, 0, wxALIGN_CENTRE_HORIZONTAL);

		for(int i=0, count=comparisonList->GetColumnCount(); i<count; ++i)
		{
			comparisonList->SetColumnWidth(i, wxLIST_AUTOSIZE_USEHEADER);
		}

		wxFlexGridSizer *bottomSizer = new wxFlexGridSizer(1, 0, 0, 5);
			bottomSizer->Add(new wxButton(this, wxID_ANY, wxT("&Save Table...")));
			bottomSizer->Add(new wxButton(this, wxID_CANCEL, wxT("&Close")));
		innerSizer->Add(bottomSizer, 0, wxALIGN_CENTRE_HORIZONTAL);

		sizer->Fit(this);
		CentreOnParent();
	}
};

}

#endif
