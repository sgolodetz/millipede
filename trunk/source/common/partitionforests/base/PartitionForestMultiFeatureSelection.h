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
	typedef boost::shared_ptr<const PartitionForestMultiFeatureSelectionT> PartitionForestMultiFeatureSelection_CPtr;
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
		void selection_changed(int commandDepth)													{ m_listeners->multi_feature_selection_changed(commandDepth); }
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
	:	m_commandManager(new BasicCommandManager),
		m_forest(forest),
		m_listeners(new CompositeListener)
	{}

	//#################### DESTRUCTOR ####################
public:
	virtual ~PartitionForestMultiFeatureSelection()
	{}

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
public:
	PartitionForestMultiFeatureSelection(const PartitionForestMultiFeatureSelection& rhs)
	:	m_commandManager(rhs.m_commandManager),
		m_forest(rhs.m_forest),
		m_listeners(new CompositeListener)
	{
		for(typename std::map<Feature,PartitionForestSelection_Ptr>::const_iterator it=rhs.m_selections.begin(), iend=rhs.m_selections.end(); it!=iend; ++it)
		{
			*selection_internal(it->first) = *it->second;
		}
	}

private:
	// Currently deliberately left unimplemented (not needed)
	PartitionForestMultiFeatureSelection& operator=(const PartitionForestMultiFeatureSelection&);

	//#################### PUBLIC METHODS ####################
public:
	void add_shared_listener(const shared_ptr<Listener>& listener)
	{
		m_listeners->add_shared_listener(listener);
	}

	void add_weak_listener(const weak_ptr<Listener>& listener)
	{
		m_listeners->add_weak_listener(listener);
	}

	void clear_all()
	{
		SequenceGuard guard(m_commandManager, "Clear All Features", m_listeners);
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

	void combine(const PartitionForestMultiFeatureSelection_CPtr& lhs, const PartitionForestMultiFeatureSelection_CPtr& rhs)
	{
		// Note: This method should only be invoked on newly-created multi-feature selections.
		assert(m_selections.empty());

		typedef std::map<Feature,PartitionForestSelection_Ptr> SelectionMap;
		typedef typename SelectionMap::const_iterator SelectionMapCIter;

		// Step 1: Construct the appropriate selections.
		for(SelectionMapCIter it=lhs->m_selections.begin(), iend=lhs->m_selections.end(); it!=iend; ++it)
		{
			selection(it->first);
		}

		for(SelectionMapCIter it=rhs->m_selections.begin(), iend=rhs->m_selections.end(); it!=iend; ++it)
		{
			selection(it->first);
		}

		// Step 2: Combine the corresponding selections from lhs and rhs.
		for(SelectionMapCIter it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			const Feature& feature = it->first;
			const PartitionForestSelection_Ptr& selection = it->second;

			SelectionMapCIter lhsSelIt = lhs->m_selections.find(feature);
			SelectionMapCIter rhsSelIt = rhs->m_selections.find(feature);

			if(lhsSelIt == lhs->m_selections.end())			*selection = *rhsSelIt->second;
			else if(rhsSelIt == rhs->m_selections.end())	*selection = *lhsSelIt->second;
			else											selection->combine(lhsSelIt->second, rhsSelIt->second);
		}
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

	bool has_selection(const Feature& feature) const
	{
		return m_selections.find(feature) != m_selections.end();
	}

	void identify_multi_feature_selection(const PartitionForestMultiFeatureSelection_CPtr& mfs)
	{
		SequenceGuard guard(m_commandManager, "Identify Multi-Feature Selection", m_listeners);
		for(typename std::map<Feature,PartitionForestSelection_Ptr>::const_iterator it=mfs->m_selections.begin(), iend=mfs->m_selections.end(); it!=iend; ++it)
		{
			identify_selection(it->second, it->first);
		}
	}

	void identify_node(const PFNodeID& node, const Feature& feature)
	{
		selection_internal(feature)->select_node(node);
	}

	void identify_selection(const PartitionForestSelection_CPtr& selection, const Feature& feature)
	{
		SequenceGuard guard(m_commandManager, "Identify Selection", m_listeners);
		for(typename PartitionForestSelectionT::NodeConstIterator it=selection->nodes_cbegin(), iend=selection->nodes_cend(); it!=iend; ++it)
		{
			identify_node(*it, feature);
		}
	}

	shared_ptr<CompositeListener> listeners() const
	{
		return m_listeners;
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

	void subtract(const PartitionForestMultiFeatureSelection_CPtr& lhs, const PartitionForestMultiFeatureSelection_CPtr& rhs)
	{
		// Note: This method should only be invoked on newly-created multi-feature selections.
		assert(m_selections.empty());

		typedef std::map<Feature,PartitionForestSelection_Ptr> SelectionMap;
		typedef typename SelectionMap::const_iterator SelectionMapCIter;

		// Step 1: Construct the appropriate selections.
		for(SelectionMapCIter it=lhs->m_selections.begin(), iend=lhs->m_selections.end(); it!=iend; ++it)
		{
			selection(it->first);
		}

		// Step 2: Subtract the corresponding rhs selections from the lhs ones.
		for(SelectionMapCIter it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			const Feature& feature = it->first;
			const PartitionForestSelection_Ptr& selection = it->second;

			SelectionMapCIter lhsSelIt = lhs->m_selections.find(feature);
			SelectionMapCIter rhsSelIt = rhs->m_selections.find(feature);

			if(rhsSelIt != rhs->m_selections.end())		selection->subtract(lhsSelIt->second, rhsSelIt->second);
			else										*selection = *lhsSelIt->second;
		}
	}

	void toggle_node(const PFNodeID& node, const Feature& feature)
	{
		selection_internal(feature)->toggle_node(node);
	}

	void toggle_selection(const PartitionForestSelection_CPtr& selection, const Feature& feature)
	{
		SequenceGuard guard(m_commandManager, "Toggle Selection", m_listeners);
		for(typename PartitionForestSelectionT::NodeConstIterator it=selection->nodes_cbegin(), iend=selection->nodes_cend(); it!=iend; ++it)
		{
			toggle_node(*it, feature);
		}
	}

	void unidentify_node(const PFNodeID& node, const Feature& feature)
	{
		selection_internal(feature)->deselect_node(node);
	}

	void unidentify_selection(const PartitionForestSelection_CPtr& selection, const Feature& feature)
	{
		SequenceGuard guard(m_commandManager, "Unidentify Selection", m_listeners);
		for(typename PartitionForestSelectionT::NodeConstIterator it=selection->nodes_cbegin(), iend=selection->nodes_cend(); it!=iend; ++it)
		{
			unidentify_node(*it, feature);
		}
	}

	void write_text(std::ostream& os)
	{
		os << "{\n";
		for(typename std::map<Feature,PartitionForestSelection_Ptr>::iterator it=m_selections.begin(), iend=m_selections.end(); it!=iend; ++it)
		{
			os << feature_name(it->first) << '\n';
			it->second->write_text(os);
		}
		os << "}\n";
	}

	//#################### PRIVATE METHODS ####################
private:
	PartitionForestSelection_Ptr selection_internal(const Feature& feature)
	{
		return boost::const_pointer_cast<PartitionForestSelectionT>(selection(feature));
	}
};

}

using mp_PartitionForestMultiFeatureSelection::PartitionForestMultiFeatureSelection;

}

#endif
