/***
 * millipede: SelectionView.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SELECTIONVIEW
#define H_MILLIPEDE_SELECTIONVIEW

#include <map>
#include <sstream>

#include <boost/lexical_cast.hpp>

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
	struct MFSManagerListener : PartitionModelT::PartitionForestMFSManagerT::Listener
	{
		SelectionView *base;

		explicit MFSManagerListener(SelectionView *base_)
		:	base(base_)
		{}

		void multi_feature_selection_manager_changed()
		{
			base->add_mfs_listener();
			base->refresh_list();
		}
	};

	struct ModelListener : PartitionModelT::Listener
	{
		SelectionView *base;

		explicit ModelListener(SelectionView *base_)
		:	base(base_)
		{}

		void forest_changed()
		{
			base->refresh_list();
			base->add_listeners();
		}
	};

	struct MultiFeatureSelectionListener : PartitionModelT::VolumeIPFMultiFeatureSelectionT::Listener
	{
		SelectionView *base;

		explicit MultiFeatureSelectionListener(SelectionView *base_)
		:	base(base_)
		{}

		void multi_feature_selection_changed(int commandDepth)
		{
			if(commandDepth == 0) base->refresh_list();
		}
	};

	struct SelectionListener : PartitionModelT::VolumeIPFSelectionT::Listener
	{
		SelectionView *base;

		explicit SelectionListener(SelectionView *base_)
		:	base(base_)
		{}

		void selection_changed(int commandDepth)
		{
			if(commandDepth == 0) base->refresh_list();
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	std::map<std::string,size_t> m_columnMap;
	PartitionModel_Ptr m_model;
	boost::shared_ptr<MultiFeatureSelectionListener> m_multiFeatureSelectionListener;

	//#################### CONSTRUCTORS ####################
public:
	SelectionView(wxWindow *parent, const PartitionModel_Ptr& model)
	:	wxListCtrl(parent, wxID_ANY, wxDefaultPosition, wxSize(1000,200), wxLC_REPORT|wxLC_SINGLE_SEL|wxLC_HRULES|wxLC_VRULES), m_model(model)
	{
		model->add_shared_listener(boost::shared_ptr<ModelListener>(new ModelListener(this)));

		// Set up the columns.
		add_column("Node ID");
		add_column("Features");
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

	void add_mfs_listener()
	{
		m_multiFeatureSelectionListener.reset(new MultiFeatureSelectionListener(this));
		m_model->active_multi_feature_selection()->add_weak_listener(m_multiFeatureSelectionListener);
	}

	void add_listeners()
	{
		m_model->selection()->add_shared_listener(boost::shared_ptr<SelectionListener>(new SelectionListener(this)));
		m_model->multi_feature_selection_manager()->add_shared_listener(boost::shared_ptr<MFSManagerListener>(new MFSManagerListener(this)));
		add_mfs_listener();
	}

	void refresh_list()
	{
		DeleteAllItems();

		typename PartitionModelT::VolumeIPFSelection_CPtr selection = m_model->selection();
		typename PartitionModelT::VolumeIPFMultiFeatureSelection_CPtr multiFeatureSelection = m_model->active_multi_feature_selection();
		int index = 0;

		typedef typename PartitionModelT::VolumeIPFSelectionT::NodeConstIterator Iter;
		for(Iter it=selection->nodes_cbegin(), iend=selection->nodes_cend(); it!=iend; ++it, ++index)
		{
			InsertItem(index, string_to_wxString(boost::lexical_cast<std::string>(*it)));

			std::vector<Feature> features = multiFeatureSelection->features_of(*it);
			std::ostringstream oss;
			for(size_t j=0, size=features.size(); j<size; ++j)
			{
				oss << feature_name(features[j]);
				if(j < size-1) oss << ", ";
			}
			SetItem(index, m_columnMap.find("Features")->second, string_to_wxString(oss.str()));

			if(it->layer() > 0)
			{
				std::map<std::string,std::string> propertyMap = m_model->volume_ipf()->branch_properties(*it).property_map();
				for(std::map<std::string,std::string>::const_iterator jt=propertyMap.begin(), jend=propertyMap.end(); jt!=jend; ++jt)
				{
					int column = m_columnMap.find(jt->first)->second;
					SetItem(index, column, string_to_wxString(jt->second));
				}
			}
		}

		for(int i=0, count=GetColumnCount(); i<count; ++i)
		{
			SetColumnWidth(i, wxLIST_AUTOSIZE_USEHEADER);
		}
	}
};

}

#endif
