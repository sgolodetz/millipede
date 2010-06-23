/***
 * millipede: PartitionForestMultiFeatureSelection.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFORESTMULTIFEATURESELECTION
#define H_MILLIPEDE_PARTITIONFORESTMULTIFEATURESELECTION

#include <map>

#include <common/commands/CommandSequenceGuard.h>
#include "PartitionForestSelection.h"

namespace mp {

namespace mp_PartitionForestMultiFeatureSelection {

using namespace boost;

template <typename LeafLayer, typename BranchLayer, typename Feature>
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

	typedef PartitionForestMultiFeatureSelection<LeafLayer,BranchLayer,Feature> PartitionForestMultiFeatureSelectionT;

	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_commandManager;
	PartitionForest_Ptr m_forest;
	mutable std::map<Feature,PartitionForestSelection_Ptr> m_selections;

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
		for(typename std::map<Feature,PartitionForestSelection_Ptr>::iterator it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			it->second->clear();
		}
	}

	void clear_feature(const Feature& feature)
	{
		typename std::map<Feature,PartitionForestSelection_Ptr>::iterator it = m_selections.find(feature);
		if(it != m_selections.end()) it->second->clear();
	}

	bool empty() const
	{
		for(typename std::map<Feature,PartitionForestSelection_Ptr>::iterator it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			if(!it->second->empty()) return false;
		}
		return true;
	}

	std::vector<Feature> features_of(const PFNodeID& node) const
	{
		std::vector<Feature> ret;
		for(typename std::map<Feature,PartitionForestSelection_Ptr>::const_iterator it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			if(it->second->contains(node)) ret.push_back(it->first);
		}
		return ret;
	}

	PartitionForest_CPtr forest() const
	{
		return m_forest;
	}

	void identify_feature(const PFNodeID& node, const Feature& feature)
	{
		selection(feature)->select_node(node);
	}

	PartitionForestSelection_CPtr selection(const Feature& feature) const
	{
		typename std::map<Feature,PartitionForestSelection_Ptr>::iterator it = m_selections.find(feature);
		if(it == m_selections.end())
		{
			PartitionForestSelection_Ptr selection(new PartitionForestSelectionT(m_forest));
			selection->set_command_manager(m_commandManager);
			m_forest->add_weak_listener(selection);
			it = m_selections.insert(std::make_pair(feature, selection)).first;
		}
		return it->second;
	}

	void set_command_manager(const ICommandManager_Ptr& commandManager)
	{
		m_commandManager = commandManager;
		for(typename std::map<Feature,PartitionForestSelection_Ptr>::iterator it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			it->second->set_command_manager(commandManager);
		}
	}

	void unidentify_feature(const PFNodeID& node, const Feature& feature)
	{
		typename std::map<Feature,PartitionForestSelection_Ptr>::iterator it = m_selections.find(feature);
		if(it == m_selections.end()) return;
		it->second->deselect_node(node);
	}

	//#################### PRIVATE METHODS ####################
private:
	PartitionForestSelection_Ptr selection(const Feature& feature)
	{
		PartitionForestSelection_CPtr s = const_cast<const PartitionForestMultiFeatureSelectionT*>(this)->selection(feature);
		return boost::const_pointer_cast<PartitionForestSelectionT>(s);
	}
};

}

using mp_PartitionForestMultiFeatureSelection::PartitionForestMultiFeatureSelection;

}

#endif
