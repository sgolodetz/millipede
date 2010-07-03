/***
 * millipede: SelectionView.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SELECTIONVIEW
#define H_MILLIPEDE_SELECTIONVIEW

#include <map>

#include <wx/listctrl.h>

#include <mast/models/PartitionModel.h>
#include <mast/util/StringConversion.h>

namespace mp {

template <typename LeafLayer, typename BranchLayer, typename Feature>
class SelectionView : public wxListCtrl
{
	//#################### TYPEDEFS ####################
private:
	typedef typename BranchLayer::NodeProperties BranchProperties;
	typedef PartitionModel<LeafLayer,BranchLayer,Feature> PartitionModelT;
	typedef boost::shared_ptr<PartitionModelT> PartitionModel_Ptr;

	//#################### LISTENERS ####################
private:
	struct ModelListener : PartitionModelT::Listener
	{
		SelectionView *base;

		explicit ModelListener(SelectionView *base_)
		:	base(base_)
		{}

		void forest_changed()
		{
			base->refresh_list();
			base->add_selection_listeners();
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	std::map<std::string,size_t> m_columnMap;
	PartitionModel_Ptr m_model;

	//#################### CONSTRUCTORS ####################
public:
	SelectionView(wxWindow *parent, const PartitionModel_Ptr& model)
	:	wxListCtrl(parent, wxID_ANY, wxDefaultPosition, wxSize(1000,200), wxLC_REPORT|wxLC_SINGLE_SEL|wxLC_HRULES|wxLC_VRULES), m_model(model)
	{
		model->add_shared_listener(boost::shared_ptr<ModelListener>(new ModelListener(this)));

		// Set up the columns.
		add_column("Node ID");
		add_column("Feature");
		std::vector<std::string> propertyNames = BranchProperties::property_names();
		for(size_t i=0, size=propertyNames.size(); i<size; ++i)
		{
			add_column(propertyNames[i]);
		}

		for(size_t i=0, size=m_columnMap.size(); i<size; ++i)
		{
			SetColumnWidth(i, wxLIST_AUTOSIZE_USEHEADER);
		}
	}

	//#################### PRIVATE METHODS ####################
private:
	void add_column(const std::string& title)
	{
		size_t column = m_columnMap.size();
		InsertColumn(column, string_to_wxString(title));
		m_columnMap.insert(std::make_pair(title, column));
	}

	void add_selection_listeners()
	{
		// TODO
	}

	void refresh_list()
	{
		// TODO
	}
};

}

#endif
