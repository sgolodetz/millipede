/***
 * millipede: FeatureVolumesDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FEATUREVOLUMESDIALOG
#define H_MILLIPEDE_FEATUREVOLUMESDIALOG

#include <sstream>

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/listctrl.h>
#include <wx/sizer.h>

#include <millipede/math/NumericUtil.h>
#include <mast/models/PartitionModel.h>
#include <mast/util/StringConversion.h>

namespace mp {

class FeatureVolumesDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_index;
	wxListCtrl *m_list;

	//#################### CONSTRUCTORS ####################
public:
	template <typename LeafLayer, typename BranchLayer, typename Feature>
	FeatureVolumesDialog(wxWindow *parent, const boost::shared_ptr<const PartitionModel<LeafLayer,BranchLayer,Feature> >& model)
	:	wxDialog(parent, wxID_ANY, wxT("Feature Volumes"), wxDefaultPosition, wxDefaultSize), m_index(0)
	{
		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		SetSizer(sizer);

		// Add a list control to show the volumes.
		m_list = new wxListCtrl(this, wxID_ANY, wxDefaultPosition, wxSize(300,200), wxLC_REPORT|wxLC_SINGLE_SEL|wxLC_HRULES|wxLC_VRULES);
		m_list->InsertColumn(0, wxT("Feature Name"));
		m_list->InsertColumn(1, wxT("Volume (cubic cm, 3 d.p.)"));

		for(Feature feature=enum_begin<Feature>(), end=enum_end<Feature>(); feature!=end; ++feature)
		{
			double volumeMM3 = model->active_multi_feature_selection()->voxel_count(feature) * model->dicom_volume()->voxel_size_mm3();
			add_feature_volume(feature, volumeMM3);
		}

		m_list->SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER);
		m_list->SetColumnWidth(1, wxLIST_AUTOSIZE_USEHEADER);
		sizer->Add(m_list, 0, wxALIGN_CENTRE_HORIZONTAL);

		// Add an OK button.
		wxButton *okButton = new wxButton(this, wxID_OK, wxT("OK"));
		sizer->Add(okButton, 0, wxALIGN_CENTRE_HORIZONTAL);
		okButton->SetFocus();

		sizer->Fit(this);
		CentreOnParent();
	}

	//#################### PRIVATE METHODS ####################
private:
	template <typename Feature>
	void add_feature_volume(const Feature& feature, double volumeMM3)
	{
		std::ostringstream oss;
		oss.setf(std::ios::fixed, std::ios::floatfield);
		oss.precision(3);
		oss << (volumeMM3 / 1000.0);	// the / 1000.0 converts cubic mm to cubic cm
		
		m_list->InsertItem(m_index, string_to_wxString(feature_to_name(feature)));
		m_list->SetItem(m_index, 1, string_to_wxString(oss.str()));
		++m_index;
	}
};

}

#endif
