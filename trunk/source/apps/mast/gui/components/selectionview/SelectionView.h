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

	//#################### PRIVATE VARIABLES ####################
private:
	std::map<std::string,size_t> m_columnMap;

	//#################### CONSTRUCTORS ####################
public:
	explicit SelectionView(wxWindow *parent)
	:	wxListCtrl(parent, wxID_ANY, wxDefaultPosition, wxSize(1000,200), wxLC_REPORT|wxLC_SINGLE_SEL|wxLC_HRULES|wxLC_VRULES)
	{
		// Set up the columns.
		add_column("Node ID");
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
};

}

#endif
