/***
 * millipede: PartitionForestMultiFeatureSelection.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFORESTMULTIFEATURESELECTION
#define H_MILLIPEDE_PARTITIONFORESTMULTIFEATURESELECTION

#include <map>

#include <common/commands/ListenerAlertingCommandSequenceGuard.h>
#include "PartitionForestSelection.h"

namespace mp {

namespace mp_PartitionForestMultiFeatureSelection {

using namespace boost;

template <typename LeafLayer, typename BranchLayer, typename FeatureType>
class PartitionForestMultiFeatureSelection
{
	//#################### TYPEDEFS ####################
public:
	typedef FeatureType Feature;
private:
	typedef PartitionForest<LeafLayer,BranchLayer> PartitionForestT;
	typedef shared_ptr<PartitionForestT> PartitionForest_Ptr;
	typedef shared_ptr<const PartitionForestT> PartitionForest_CPtr;

	typedef PartitionForestSelection<LeafLayer,BranchLayer> PartitionForestSelectionT;
	typedef shared_ptr<PartitionForestSelectionT> PartitionForestSelection_Ptr;
	typedef shared_ptr<const PartitionForestSelectionT> PartitionForestSelection_CPtr;

	typedef PartitionForestMultiFeatureSelection<LeafLayer,BranchLayer,Feature> PartitionForestMultiFeatureSelectionT;
public:
	typedef typename PartitionForestSelectionT::Modification Modification;

	//#################### LISTENERS ####################
public:
	struct Listener
	{
		virtual ~Listener()																								{}
		virtual void command_sequence_execution_began(const std::string& description, int commandDepth)					{}
		virtual void command_sequence_execution_ended(const std::string& description, int commandDepth)					{ multi_feature_selection_changed(commandDepth); }
		virtual void command_sequence_undo_began(const std::string& description, int commandDepth)						{}
		virtual void command_sequence_undo_ended(const std::string& description, int commandDepth)						{ multi_feature_selection_changed(commandDepth); }
		virtual void feature_was_cleared(const Feature& feature, int commandDepth)										{ multi_feature_selection_changed(commandDepth); }
		virtual void modification_redone(const Modification& modification, const Feature& feature, int commandDepth)	{ multi_feature_selection_changed(commandDepth); }
		virtual void modification_undone(const Modification& modification, const Feature& feature, int commandDepth)	{ multi_feature_selection_changed(commandDepth); }
		virtual void multi_feature_selection_changed(int commandDepth)													{}
		virtual void node_was_identified(const PFNodeID& node, const Feature& feature, int commandDepth)				{ multi_feature_selection_changed(commandDepth); }
		virtual void node_was_unidentified(const PFNodeID& node, const Feature& feature, int commandDepth)				{ multi_feature_selection_changed(commandDepth); }
	};

	typedef ListenerAlertingCommandSequenceGuard<Listener> SequenceGuard;

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void command_sequence_execution_began(const std::string& description, int commandDepth)					{ multicast(bind(&Listener::command_sequence_execution_began, _1, description, commandDepth)); }
		void command_sequence_execution_ended(const std::string& description, int commandDepth)					{ multicast(bind(&Listener::command_sequence_execution_ended, _1, description, commandDepth)); }
		void command_sequence_undo_began(const std::string& description, int commandDepth)						{ multicast(bind(&Listener::command_sequence_undo_began, _1, description, commandDepth)); }
		void command_sequence_undo_ended(const std::string& description, int commandDepth)						{ multicast(bind(&Listener::command_sequence_undo_ended, _1, description, commandDepth)); }
		void feature_was_cleared(const Feature& feature, int commandDepth)										{ multicast(bind(&Listener::feature_was_cleared, _1, feature, commandDepth)); }
		void modification_redone(const Modification& modification, const Feature& feature, int commandDepth)	{ multicast(bind(&Listener::modification_redone, _1, modification, feature, commandDepth)); }
		void modification_undone(const Modification& modification, const Feature& feature, int commandDepth)	{ multicast(bind(&Listener::modification_undone, _1, modification, feature, commandDepth)); }
		void multi_feature_selection_changed(int commandDepth)													{ multicast(bind(&Listener::multi_feature_selection_changed, _1, commandDepth)); }
		void node_was_identified(const PFNodeID& node, const Feature& feature, int commandDepth)				{ multicast(bind(&Listener::node_was_identified, _1, node, feature, commandDepth)); }
		void node_was_unidentified(const PFNodeID& node, const Feature& feature, int commandDepth)				{ multicast(bind(&Listener::node_was_unidentified, _1, node, feature, commandDepth)); }
	};

	struct SelectionListener : PartitionForestSelectionT::Listener
	{
		Feature m_feature;
		shared_ptr<CompositeListener> m_listeners;

		SelectionListener(const Feature& feature, const shared_ptr<CompositeListener>& listeners)
		:	m_feature(feature), m_listeners(listeners)
		{}

		void command_sequence_execution_began(const std::string& description, int commandDepth)		{ m_listeners->command_sequence_execution_began(description, commandDepth); }
		void command_sequence_execution_ended(const std::string& description, int commandDepth)		{ m_listeners->command_sequence_execution_ended(description, commandDepth); }
		void command_sequence_undo_began(const std::string& description, int commandDepth)			{ m_listeners->command_sequence_undo_began(description, commandDepth); }
		void command_sequence_undo_ended(const std::string& description, int commandDepth)			{ m_listeners->command_sequence_undo_ended(description, commandDepth); }
		void modification_redone(const Modification& modification, int commandDepth)				{ m_listeners->modification_redone(modification, m_feature, commandDepth); }
		void modification_undone(const Modification& modification, int commandDepth)				{ m_listeners->modification_undone(modification, m_feature, commandDepth); }
		void node_was_deselected(const PFNodeID& node, int commandDepth)							{ m_listeners->node_was_unidentified(node, m_feature, commandDepth); }
		void node_was_selected(const PFNodeID& node, int commandDepth)								{ m_listeners->node_was_identified(node, m_feature, commandDepth); }
		void selection_was_cleared(int commandDepth)												{ m_listeners->feature_was_cleared(m_feature, commandDepth); }
	};

	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_commandManager;
	PartitionForest_Ptr m_forest;
	shared_ptr<CompositeListener> m_listeners;
	mutable std::map<Feature,PartitionForestSelection_Ptr> m_selections;

	//#################### CONSTRUCTORS ####################
public:
	explicit PartitionForestMultiFeatureSelection(const PartitionForest_Ptr& forest)
	:	m_commandManager(new BasicCommandManager), m_forest(forest), m_listeners(new CompositeListener)
	{}

	//#################### DESTRUCTOR ####################
public:
	virtual ~PartitionForestMultiFeatureSelection()
	{}

	//#################### PUBLIC METHODS ####################
public:
	void add_shared_listener(const shared_ptr<Listener>& listener)
	{
		m_listeners->add_shared_listener(listener);
	}

	void clear_all()
	{
		SequenceGuard guard(m_commandManager, m_listeners, "Clear All Features");
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

	void identify_node(const PFNodeID& node, const Feature& feature)
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
			selection->add_shared_listener(boost::shared_ptr<SelectionListener>(new SelectionListener(feature, m_listeners)));
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

	void unidentify_node(const PFNodeID& node, const Feature& feature)
	{
		selection(feature)->deselect_node(node);
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
