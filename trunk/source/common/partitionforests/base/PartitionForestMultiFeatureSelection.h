/***
 * millipede: PartitionForestMultiFeatureSelection.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFORESTMULTIFEATURESELECTION
#define H_MILLIPEDE_PARTITIONFORESTMULTIFEATURESELECTION

#include <map>

#include "PartitionForestSelection.h"

namespace mp {

namespace mp_PartitionForestMultiFeatureSelection {

using namespace boost;

template <typename LeafLayer, typename BranchLayer, typename FeatureID>
class PartitionForestMultiFeatureSelection
{
	//#################### TYPEDEFS ####################
private:
	typedef PartitionForest<LeafLayer,BranchLayer> PartitionForestT;
	typedef shared_ptr<PartitionForestT> PartitionForest_Ptr;
	typedef shared_ptr<const PartitionForestT> PartitionForest_CPtr;

	typedef PartitionForestSelection<LeafLayer,BranchLayer> PartitionForestSelectionT;
	typedef shared_ptr<PartitionForestSelectionT> PartitionForestSelection_Ptr;
	typedef shared_ptr<const PartitionForestSelectionT> PartitionForestSelection_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_commandManager;
	PartitionForest_Ptr m_forest;
	mutable std::map<FeatureID,PartitionForestSelection_Ptr> m_selections;

	//#################### CONSTRUCTORS ####################
public:
	explicit PartitionForestMultiFeatureSelection(const PartitionForest_Ptr& forest)
	:	m_commandManager(new BasicCommandManager), m_forest(forest)
	{}

	//#################### DESTRUCTOR ####################
public:
	virtual ~PartitionForestMultiFeatureSelection()
	{}

	//#################### PUBLIC METHODS ####################
public:
	void clear_all()
	{
		CommandSequenceGuard guard(m_commandManager, "Clear All Features");
		for(typename std::map<FeatureID,PartitionForestSelection_Ptr>::iterator it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			it->second->clear();
		}
	}

	void clear_feature(FeatureID featureID)
	{
		typename std::map<FeatureID,PartitionForestSelection_Ptr>::iterator it = m_selections.find(featureID);
		if(it != m_selections.end()) it->second->clear();
	}

	bool empty() const
	{
		for(typename std::map<FeatureID,PartitionForestSelection_Ptr>::iterator it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			if(!it->second->empty()) return false;
		}
		return true;
	}

	std::vector<FeatureID> features_of(const PFNodeID& node) const
	{
		std::vector<FeatureID> ret;
		for(typename std::map<FeatureID,PartitionForestSelection_Ptr>::const_iterator it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			if(it->second->contains(node)) ret.push_back(it->first);
		}
		return ret;
	}

	PartitionForest_CPtr forest() const
	{
		return m_forest;
	}

	void identify_feature(const PFNodeID& node, FeatureID featureID)
	{
		typename std::map<FeatureID,PartitionForestSelection_Ptr>::iterator it = m_selections.find(featureID);
		if(it == m_selections.end())
		{
			PartitionForestSelection_Ptr selection(new PartitionForestSelectionT(m_forest));
			selection->set_command_manager(m_commandManager);
			m_forest->add_listener(selection);
			it = m_selections.insert(std::make_pair(featureID, selection)).first;
		}
		const PartitionForestSelection_Ptr& selection = it->second;
		selection->select_node(node);
	}

	PartitionForestSelection_CPtr selection(FeatureID featureID) const
	{
		return m_selections[featureID];
	}

	void set_command_manager(const ICommandManager_Ptr& commandManager)
	{
		m_commandManager = commandManager;
		for(typename std::map<FeatureID,PartitionForestSelection_Ptr>::iterator it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			it->second->set_command_manager(commandManager);
		}
	}

	void unidentify_feature(const PFNodeID& node, FeatureID featureID)
	{
		typename std::map<FeatureID,PartitionForestSelection_Ptr>::iterator it = m_selections.find(featureID);
		if(it == m_selections.end()) return;
		it->second->deselect_node(node);
	}
};

}

using mp_PartitionForestMultiFeatureSelection::PartitionForestMultiFeatureSelection;

}

#endif
