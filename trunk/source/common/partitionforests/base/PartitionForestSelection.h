/***
 * millipede: PartitionForestSelection.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFORESTSELECTION
#define H_MILLIPEDE_PARTITIONFORESTSELECTION

#include <algorithm>
#include <numeric>
#include <stack>

#include <iostream>

#include <boost/function.hpp>
#include <boost/lexical_cast.hpp>

#include <common/io/util/LineIO.h>
#include "PartitionForest.h"

namespace mp {

namespace mp_PartitionForestSelection {

using namespace boost;

template <typename LeafLayer, typename BranchLayer>
class PartitionForestSelection : public PartitionForest<LeafLayer,BranchLayer>::Listener
{
	//#################### TYPEDEFS ####################
private:
	typedef std::set<int> Layer;
	typedef PartitionForest<LeafLayer,BranchLayer> PartitionForestT;
	typedef shared_ptr<PartitionForestT> PartitionForest_Ptr;
	typedef shared_ptr<const PartitionForestT> PartitionForest_CPtr;
	typedef PartitionForestSelection<LeafLayer,BranchLayer> PartitionForestSelectionT;
	typedef shared_ptr<const PartitionForestSelectionT> PartitionForestSelection_CPtr;

	//#################### NESTED CLASSES (EXCLUDING COMMANDS, ITERATORS AND LISTENERS) ####################
public:
	class Modification
	{
	private:
		std::set<PFNodeID> m_erased;
		std::set<PFNodeID> m_inserted;

	public:
		void erase_node(const PFNodeID& node)
		{
			std::set<PFNodeID>::iterator it = m_inserted.find(node);
			if(it != m_inserted.end()) m_inserted.erase(it);
			else m_erased.insert(node);
		}

		const std::set<PFNodeID>& erased_nodes() const
		{
			return m_erased;
		}

		void insert_node(const PFNodeID& node)
		{
			//std::cout << "Inserting node (from selection class)" << std::endl;
			std::set<PFNodeID>::iterator it = m_erased.find(node);
			if(it != m_erased.end()) m_erased.erase(it);
			else m_inserted.insert(node);
		}

		const std::set<PFNodeID>& inserted_nodes() const
		{
			return m_inserted;
		}
	};

	//#################### COMMANDS ####################
private:
	struct ModifyingCommand : Command
	{
		typedef boost::function<Modification (PartitionForestSelection*,int)> ModifyingFunction;

		PartitionForestSelection *m_base;
		ModifyingFunction m_function;
		Modification m_modification;

		ModifyingCommand(PartitionForestSelection *base, const ModifyingFunction& function, const std::string& description)
		:	Command(description), m_base(base), m_function(function)
		{}

		void execute()	{ m_modification = m_function(m_base, depth()); }
		void redo()		{ m_base->redo_modification(m_modification, depth()); }
		void undo()		{ m_base->undo_modification(m_modification, depth()); }
	};

	//#################### ITERATORS ####################
public:
	class NodeConstIterator : public std::iterator<std::input_iterator_tag, PFNodeID>
	{
	private:
		const PartitionForestSelection *m_base;
		int m_layerIndex;
		Layer::const_iterator m_it;
		mutable shared_ptr<PFNodeID> m_currentNode;
	public:
		NodeConstIterator(const PartitionForestSelection *base, bool end)
		:	m_base(base)
		{
			if(!end)
			{
				m_layerIndex = static_cast<int>(m_base->m_nodes.size()) - 1;
				assert(m_layerIndex >= 0);	// the partition forest always has at least one layer
				m_it = m_base->m_nodes[m_layerIndex].begin();
				find_next_node();
			}
			else
			{
				m_layerIndex = -1;
				m_it = m_base->m_nodes[0].end();
			}
		}

		const PFNodeID& operator*() const
		{
			if(!m_currentNode) m_currentNode.reset(new PFNodeID(m_layerIndex, *m_it));
			return *m_currentNode;
		}

		const PFNodeID *operator->() const
		{
			return &operator*();
		}

		NodeConstIterator& operator++()
		{
			++m_it;
			find_next_node();
			m_currentNode.reset();
			return *this;
		}

		bool operator==(const NodeConstIterator& rhs) const
		{
			return m_layerIndex == rhs.m_layerIndex && m_it == rhs.m_it;
		}

		bool operator!=(const NodeConstIterator& rhs) const
		{
			return !(*this == rhs);
		}
	private:
		void find_next_node()
		{
			while(m_it == m_base->m_nodes[m_layerIndex].end())
			{
				--m_layerIndex;
				if(m_layerIndex == -1) break;
				m_it = m_base->m_nodes[m_layerIndex].begin();
			}

			// At this point, either m_it points to a valid node, or m_layerIndex == -1 and m_it == m_nodes[0].end().
		}
	};

	//iterates over ..?
	class ViewNodeConstIterator : public std::iterator<std::input_iterator_tag, PFNodeID>
	{
	private:
		const PartitionForestSelection *m_base;
		int m_viewLayer;
		NodeConstIterator m_nodeIt;
		mutable std::list<PFNodeID> m_descendants;
	public:
		ViewNodeConstIterator(const PartitionForestSelection *base, int viewLayer, bool end)
		:	m_base(base), m_viewLayer(viewLayer), m_nodeIt(base, end)
		{
			if(!end) check_for_descendants();
		}

		const PFNodeID& operator*() const
		{
			if(m_descendants.empty()) return *m_nodeIt;
			else return m_descendants.front();
		}

		const PFNodeID *operator->() const
		{
			return &operator*();
		}

		ViewNodeConstIterator& operator++()
		{
			if(!m_descendants.empty())
			{
				m_descendants.pop_front();
			}

			if(m_descendants.empty())
			{
				++m_nodeIt;
				check_for_descendants();
			}

			return *this;
		}

		bool operator==(const ViewNodeConstIterator& rhs) const
		{
			return m_nodeIt == rhs.m_nodeIt && m_descendants.size() == rhs.m_descendants.size();
		}

		bool operator!=(const ViewNodeConstIterator& rhs) const
		{
			return !(*this == rhs);
		}
	private:
		void check_for_descendants()
		{
			if(m_nodeIt != m_base->nodes_cend())
			{
				const PFNodeID& node = *m_nodeIt;
				if(node.layer() > m_viewLayer)
				{
					m_descendants = m_base->descendants_in_layer(node, m_viewLayer);
				}
			}
		}
	};

	//#################### LISTENERS ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void command_sequence_execution_began(const std::string& description, int commandDepth)			{}
		virtual void command_sequence_execution_ended(const std::string& description, int commandDepth)			{ selection_changed(commandDepth); }
		virtual void command_sequence_undo_began(const std::string& description, int commandDepth)				{}
		virtual void command_sequence_undo_ended(const std::string& description, int commandDepth)				{ selection_changed(commandDepth); }
		virtual void modification_redone(const Modification& modification, int commandDepth)					{ selection_changed(commandDepth); }
		virtual void modification_undone(const Modification& modification, int commandDepth)					{ selection_changed(commandDepth); }
		virtual void node_was_consolidated(const PFNodeID& node)												{}
		virtual void node_was_deconsolidated(const PFNodeID& node)												{}
		virtual void node_was_deselected(const PFNodeID& node, int commandDepth)								{ selection_changed(commandDepth); }
		virtual void node_was_selected(const PFNodeID& node, int commandDepth)									{ selection_changed(commandDepth); }
		virtual void selection_changed(int commandDepth)														{}
		virtual void selection_was_cleared(int commandDepth)													{ selection_changed(commandDepth); }
		virtual void selection_was_replaced(const PartitionForestSelection_CPtr& selection, int commandDepth)	{ selection_changed(commandDepth); }
	};

	typedef ListenerAlertingCommandSequenceGuard<Listener> SequenceGuard;

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void command_sequence_execution_began(const std::string& description, int commandDepth)			{ multicast(bind(&Listener::command_sequence_execution_began, _1, description, commandDepth)); }
		void command_sequence_execution_ended(const std::string& description, int commandDepth)			{ multicast(bind(&Listener::command_sequence_execution_ended, _1, description, commandDepth)); }
		void command_sequence_undo_began(const std::string& description, int commandDepth)				{ multicast(bind(&Listener::command_sequence_undo_began, _1, description, commandDepth)); }
		void command_sequence_undo_ended(const std::string& description, int commandDepth)				{ multicast(bind(&Listener::command_sequence_undo_ended, _1, description, commandDepth)); }
		void modification_redone(const Modification& modification, int commandDepth)					{ multicast(bind(&Listener::modification_redone, _1, modification, commandDepth)); }
		void modification_undone(const Modification& modification, int commandDepth)					{ multicast(bind(&Listener::modification_undone, _1, modification, commandDepth)); }
		void node_was_consolidated(const PFNodeID& node)												{ multicast(bind(&Listener::node_was_consolidated, _1, node)); }
		void node_was_deconsolidated(const PFNodeID& node)												{ multicast(bind(&Listener::node_was_deconsolidated, _1, node)); }
		void node_was_deselected(const PFNodeID& node, int commandDepth)								{ multicast(bind(&Listener::node_was_deselected, _1, node, commandDepth)); }
		void node_was_selected(const PFNodeID& node, int commandDepth)									{ multicast(bind(&Listener::node_was_selected, _1, node, commandDepth)); }
		void selection_changed(int commandDepth)														{ multicast(bind(&Listener::selection_changed, _1, commandDepth)); }
		void selection_was_cleared(int commandDepth)													{ multicast(bind(&Listener::selection_was_cleared, _1, commandDepth)); }
		void selection_was_replaced(const PartitionForestSelection_CPtr& selection, int commandDepth)	{ multicast(bind(&Listener::selection_was_replaced, _1, selection, commandDepth)); }
	};

	//#################### PRIVATE VARIABLES ####################
private:
	// Datatype Invariant: If a node is part of the selection, then no ancestor or descendant of it is also part of the selection.
	std::vector<Layer> m_nodes;

	ICommandManager_Ptr m_commandManager;
	PartitionForest_Ptr m_forest;
	shared_ptr<CompositeListener> m_listeners;

	//#################### CONSTRUCTORS ####################
public:
	explicit PartitionForestSelection(const PartitionForest_Ptr& forest)
	:	m_commandManager(new BasicCommandManager),
		m_forest(forest),
		m_listeners(new CompositeListener)
	{
		m_nodes.resize(m_forest->highest_layer() + 1);
	}

	PartitionForestSelection(const PartitionForest_Ptr& forest, const std::set<int>& leaves)
	:	m_commandManager(new BasicCommandManager),
		m_forest(forest),
		m_listeners(new CompositeListener)
	{
		m_nodes.resize(m_forest->highest_layer() + 1);
		m_nodes[0] = leaves;
		consolidate_all();
	}

	//#################### DESTRUCTOR ####################
public:
	virtual ~PartitionForestSelection()
	{}

	//#################### COPY CONSTRUCTOR, ASSIGNMENT OPERATOR AND SWAP ####################
public:
	PartitionForestSelection(const PartitionForestSelection& rhs)
	:	m_nodes(rhs.m_nodes),
		m_commandManager(rhs.m_commandManager),
		m_forest(rhs.m_forest),
		m_listeners(new CompositeListener)
	{}

	PartitionForestSelection& operator=(const PartitionForestSelection& rhs)
	{
		PartitionForestSelection(rhs).swap(*this);
		return *this;
	}

	PartitionForestSelection& swap(PartitionForestSelection& rhs)
	{
		std::swap(m_nodes, rhs.m_nodes);
		std::swap(m_commandManager, rhs.m_commandManager);
		std::swap(m_forest, rhs.m_forest);
		std::swap(m_listeners, rhs.m_listeners);
		return *this;
	}

	//#################### PUBLIC METHODS ####################
public:
	void add_shared_listener(const shared_ptr<Listener>& listener)
	{
		m_listeners->add_shared_listener(listener);
	}

	void clear()
	{
		m_commandManager->execute(Command_Ptr(new ModifyingCommand(this, boost::bind(boost::mem_fn(&PartitionForestSelectionT::clear_impl), _1, _2), "Clear Selection")));
	}

	void combine(const PartitionForestSelection_CPtr& lhs, const PartitionForestSelection_CPtr& rhs)
	{
		// Note: This method should only be invoked on newly-created selections.
		assert(empty());

		*this = *lhs;

		for(NodeConstIterator it=rhs->nodes_cbegin(), iend=rhs->nodes_cend(); it!=iend; ++it)
		{
			select_node_impl(*it, -1);
		}
	}

	void combine_using_leaves(const PartitionForestSelection_CPtr& lhs, const PartitionForestSelection_CPtr& rhs)
	{
		// Note: This method should only be invoked on newly-created selections.
		assert(empty());

		for(NodeConstIterator it=lhs->nodes_cbegin(), iend=lhs->nodes_cend(); it!=iend; ++it)
		{
			std::deque<int> receptiveRegion = m_forest->receptive_region_of(*it);
			std::copy(receptiveRegion.begin(), receptiveRegion.end(), std::inserter(m_nodes[0], m_nodes[0].begin()));
		}

		for(NodeConstIterator it=rhs->nodes_cbegin(), iend=rhs->nodes_cend(); it!=iend; ++it)
		{
			std::deque<int> receptiveRegion = m_forest->receptive_region_of(*it);
			std::copy(receptiveRegion.begin(), receptiveRegion.end(), std::inserter(m_nodes[0], m_nodes[0].begin()));
		}

		consolidate_all();
	}

	void command_sequence_execution_ended(const std::string& description, int commandDepth)
	{
		m_listeners->selection_changed(commandDepth);
	}

	void command_sequence_undo_ended(const std::string& description, int commandDepth)
	{
		m_listeners->selection_changed(commandDepth);
	}

	bool contains(const PFNodeID& node) const
	{
		if(in_representation(node)) return true;

		std::stack<PFNodeID> trail;
		return find_ancestor_in_representation(node, trail) != PFNodeID::invalid();
	}

	void deselect_node(const PFNodeID& node)
	{
		m_commandManager->execute(Command_Ptr(new ModifyingCommand(this, boost::bind(&PartitionForestSelectionT::deselect_node_impl, _1, node, _2), "Deselect Node")));
	}

	bool empty() const
	{
		for(std::vector<Layer>::const_iterator it=m_nodes.begin(), iend=m_nodes.end(); it!=iend; ++it)
		{
			if(!it->empty()) return false;
		}
		return true;
	}

	PartitionForest_CPtr forest() const
	{
		return m_forest;
	}

	bool in_representation(const PFNodeID& node) const
	{
		if(node.layer() >= 0 && node.layer() <= m_forest->highest_layer())
		{
			const Layer& layer = m_nodes[node.layer()];
			return layer.find(node.index()) != layer.end();
		}
		else return false;
	}

	void layer_was_cloned(int index)
	{
		// The desired effect is to insert a layer above the one specified and migrate any selected nodes upwards to the new layer.
		// This can be achieved more easily by simply inserting an empty layer below the one being cloned, as here.
		m_nodes.insert(m_nodes.begin() + index, Layer());

		m_listeners->selection_changed(0);
	}

	void layer_was_deleted(int index)
	{
		m_listeners->selection_changed(0);
	}

	void layer_was_undeleted(int index)
	{
		// Re-add the layer itself.
		m_nodes.insert(m_nodes.begin() + index, Layer());

		// Construct the set of parents of selected nodes in the layer below.
		std::set<PFNodeID> parents;
		const Layer& childLayer = m_nodes[index-1];
		for(Layer::const_iterator it=childLayer.begin(), iend=childLayer.end(); it!=iend; ++it)
		{
			parents.insert(m_forest->parent_of(PFNodeID(index-1, *it)));
		}

		// Consolidate each of these parents in turn.
		for(std::set<PFNodeID>::const_iterator it=parents.begin(), iend=parents.end(); it!=iend; ++it)
		{
			consolidate_node(*it, boost::none);
		}

		m_listeners->selection_changed(0);
	}

	void layer_will_be_deleted(int index)
	{
		// Replace any nodes in the specified layer with their children in the layer below.
		Layer copy = m_nodes[index];
		for(Layer::const_iterator it=copy.begin(), iend=copy.end(); it!=iend; ++it)
		{
			deconsolidate_node(PFNodeID(index, *it), boost::none);
		}

		// Delete the layer itself.
		m_nodes.erase(m_nodes.begin() + index);
	}

	shared_ptr<CompositeListener> listeners() const
	{
		return m_listeners;
	}

	/**
	@brief	Calculates the layer in which any merging of the selected nodes should happen.

	@param[in]	viewLayer	The layer being "viewed" by the user
	@return	The layer in which a merge should happen
	*/
	int merge_layer(int viewLayer) const
	{
		for(int i=0; i<viewLayer; ++i)
		{
			if(!m_nodes[i].empty()) return i;
		}
		return viewLayer;
	}

	void node_was_split(const PFNodeID& node, const std::set<PFNodeID>& results, int commandDepth)
	{
		// Step 1:	If the node being split was itself in the selection, replace it with the results of the split.
		if(in_representation(node))
		{
			erase_node(node, boost::none);
			for(std::set<PFNodeID>::const_iterator it=results.begin(), iend=results.end(); it!=iend; ++it)
			{
				insert_node(*it, boost::none);
			}
		}

		// Step 2:	Consolidate the individual result nodes. (This is important when the split results from an unzip.)
		for(std::set<PFNodeID>::const_iterator it=results.begin(), iend=results.end(); it!=iend; ++it)
		{
			consolidate_node(*it, boost::none);
		}

		m_listeners->selection_changed(commandDepth);
	}

	NodeConstIterator nodes_cbegin() const
	{
		return NodeConstIterator(this, false);
	}

	NodeConstIterator nodes_cend() const
	{
		return NodeConstIterator(this, true);
	}

	void nodes_were_merged(const std::set<PFNodeID>& nodes, const PFNodeID& result, int commandDepth)
	{
		// Consolidate the node resulting from the merge. Note that the selection of
		// this node's ancestors in the forest will be unchanged by the merge, so we
		// don't need to consolidate those.
		consolidate_node(result, boost::none);

		m_listeners->selection_changed(commandDepth);
	}

	void nodes_will_be_merged(const std::set<PFNodeID>& nodes, int commandDepth)
	{
		// Replace any of the nodes being merged that are selected with their children in the layer below.
		for(std::set<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
		{
			if(in_representation(*it)) deconsolidate_node(*it, boost::none);
		}
	}

	void read_text(std::istream& is)
	{
		// Note: This method should only be invoked on newly-created selections.
		assert(empty());

		LineIO::read_checked_line(is, "{");
		std::string line;
		for(;;)
		{
			LineIO::read_line(is, line, "node ID");
			if(line == "}") break;
			try { insert_node(lexical_cast<PFNodeID>(line), boost::none); }
			catch(bad_lexical_cast&) {}
		}
	}

	void replace_with_node(const PFNodeID& node)
	{
		SequenceGuard guard(m_commandManager, "Replace Selection", m_listeners);
		clear();
		select_node(node);
	}

	void replace_with_selection(const PartitionForestSelection_CPtr& selection)
	{
		m_commandManager->execute(Command_Ptr(new ModifyingCommand(this, boost::bind(&PartitionForestSelectionT::replace_with_selection_impl, _1, selection, _2), "Replace With Selection")));
	}

	void select_node(const PFNodeID& node)
	{
		m_commandManager->execute(Command_Ptr(new ModifyingCommand(this, boost::bind(&PartitionForestSelectionT::select_node_impl, _1, node, _2), "Select Node")));
	}

	void set_command_manager(const ICommandManager_Ptr& commandManager)
	{
		m_commandManager = commandManager;
	}

	void subtract(const PartitionForestSelection_CPtr& lhs, const PartitionForestSelection_CPtr& rhs)
	{
		// Note: This method should only be invoked on newly-created selections.
		assert(empty());

		*this = *lhs;

		for(NodeConstIterator it=rhs->nodes_cbegin(), iend=rhs->nodes_cend(); it!=iend; ++it)
		{
			deselect_node_impl(*it, -1);
		}
	}

	void subtract_using_leaves(const PartitionForestSelection_CPtr& lhs, const PartitionForestSelection_CPtr& rhs)
	{
		// Note: This method should only be invoked on newly-created selections.
		assert(empty());

		for(NodeConstIterator it=lhs->nodes_cbegin(), iend=lhs->nodes_cend(); it!=iend; ++it)
		{
			std::deque<int> receptiveRegion = m_forest->receptive_region_of(*it);
			std::copy(receptiveRegion.begin(), receptiveRegion.end(), std::inserter(m_nodes[0], m_nodes[0].begin()));
		}

		for(NodeConstIterator it=rhs->nodes_cbegin(), iend=rhs->nodes_cend(); it!=iend; ++it)
		{
			std::deque<int> receptiveRegion = m_forest->receptive_region_of(*it);
			for(std::deque<int>::const_iterator jt=receptiveRegion.begin(), jend=receptiveRegion.end(); jt!=jend; ++jt)
			{
				m_nodes[0].erase(*jt);
			}
		}

		consolidate_all();
	}

	void toggle_node(const PFNodeID& node)
	{
		if(contains(node))	deselect_node(node);
		else				select_node(node);
	}

	ViewNodeConstIterator view_at_layer_cbegin(int layerIndex) const
	{
		return ViewNodeConstIterator(this, layerIndex, false);
	}

	ViewNodeConstIterator view_at_layer_cend(int layerIndex) const
	{
		return ViewNodeConstIterator(this, layerIndex, true);
	}

	void write_text(std::ostream& os)
	{
		os << "{\n";
		for(NodeConstIterator it=nodes_cbegin(), iend=nodes_cend(); it!=iend; ++it)
		{
			os << *it << '\n';
		}
		os << "}\n";
	}

	//#################### PRIVATE METHODS ####################
private:
	Modification clear_impl(int commandDepth)
	{
		Modification modification;
		for(int i=0, size=static_cast<int>(m_nodes.size()); i<size; ++i)
		{
			for(Layer::const_iterator jt=m_nodes[i].begin(), jend=m_nodes[i].end(); jt!=jend; ++jt)
			{
				modification.erase_node(PFNodeID(i, *jt));
			}
			m_nodes[i].clear();
		}
		m_listeners->selection_was_cleared(commandDepth);
		return modification;
	}

	void consolidate_all()
	{
		for(int i=0; i<m_forest->highest_layer(); ++i)
		{
			std::set<PFNodeID> parents;
			for(std::set<int>::const_iterator jt=m_nodes[i].begin(), jend=m_nodes[i].end(); jt!=jend; ++jt)
			{
				parents.insert(m_forest->parent_of(PFNodeID(i, *jt)));
			}

			for(std::set<PFNodeID>::const_iterator jt=parents.begin(), jend=parents.end(); jt!=jend; ++jt)
			{
				consolidate_node(*jt, boost::none);
			}
		}
	}

	bool consolidate_node(const PFNodeID& node, boost::optional<Modification&> modification)
	{
		// Check to see if all the children of the specified node are selected.
		std::set<PFNodeID> children = m_forest->children_of(node);
		for(std::set<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			if(!in_representation(*it)) return false;
		}

		// If they are, deselect them and select this node instead.
		for(std::set<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			erase_node(*it, modification);
		}
		insert_node(node, modification);

		m_listeners->node_was_consolidated(node);

		return true;
	}

	void consolidate_upwards_from_node(const PFNodeID& node, boost::optional<Modification&> modification)
	{
		PFNodeID cur = node;
		while(cur != PFNodeID::invalid() && consolidate_node(cur, modification))
		{
			cur = m_forest->parent_of(cur);
		}
	}

	void deconsolidate_node(const PFNodeID& node, boost::optional<Modification&> modification)
	{
		// Replace the selected node with its children in the forest.
		erase_node(node, modification);

		std::set<PFNodeID> children = m_forest->children_of(node);
		for(std::set<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			insert_node(*it, modification);
		}

		m_listeners->node_was_deconsolidated(node);
	}

	std::list<PFNodeID> descendants_in_layer(const PFNodeID& node, int layerIndex) const
	{
		assert(node.layer() > layerIndex);

		std::set<PFNodeID> children = m_forest->children_of(node);
		if(node.layer() != layerIndex+1)
		{
			std::list<PFNodeID> descendants;
			for(std::set<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
			{
				std::list<PFNodeID> result = descendants_in_layer(*it, layerIndex);
				descendants.splice(descendants.end(), result);
			}
			return descendants;
		}
		else return std::list<PFNodeID>(children.begin(), children.end());
	}

	std::list<PFNodeID> descendants_in_representation(const PFNodeID& node) const
	{
		std::list<PFNodeID> descendants;

		std::set<PFNodeID> children = m_forest->children_of(node);
		for(std::set<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			if(in_representation(*it))
			{
				descendants.push_back(*it);
			}
			else
			{
				std::list<PFNodeID> result = descendants_in_representation(*it);
				descendants.splice(descendants.end(), result);
			}
		}

		return descendants;
	}

	Modification deselect_node_impl(const PFNodeID& node, int commandDepth)
	{
		Modification modification;

		// Case 1:	The node itself is in the representation.
		if(in_representation(node))
		{
			erase_node(node, modification);
			m_listeners->node_was_deselected(node, commandDepth);
			return modification;
		}

		// Case 2:	An ancestor of the node is in the representation.
		std::stack<PFNodeID> trail;
		PFNodeID ancestor = find_ancestor_in_representation(node, trail);
		if(ancestor != PFNodeID::invalid())
		{
			split_selection(trail, modification);
			erase_node(node, modification);
			m_listeners->node_was_deselected(node, commandDepth);
			return modification;
		}

		// Case 3:	One or more descendants of the node are in the representation.
		std::list<PFNodeID> descendants = descendants_in_representation(node);
		if(!descendants.empty())
		{
			for(std::list<PFNodeID>::const_iterator it=descendants.begin(), iend=descendants.end(); it!=iend; ++it)
			{
				erase_node(*it, modification);
			}
			m_listeners->node_was_deselected(node, commandDepth);
			return modification;
		}

		// Case 4:	Neither the node nor any of its ancestors or descendants is in the representation.
		return modification;
	}

	void erase_node(const PFNodeID& node, boost::optional<Modification&> modification)
	{
		m_nodes[node.layer()].erase(node.index());
		if(modification) modification->erase_node(node);
	}

	PFNodeID find_ancestor_in_representation(const PFNodeID& node, std::stack<PFNodeID>& trail) const
	{
		PFNodeID parent = m_forest->parent_of(node);
		trail.push(parent);

		// If this node has no parent, then it definitely has no ancestor in the representation.
		if(parent == PFNodeID::invalid()) return PFNodeID::invalid();

		// If this node has a parent and it's in the representation, then return it.
		if(in_representation(parent)) return parent;

		// Otherwise, any ancestor this node may have is also an ancestor of its parent, so recurse.
		return find_ancestor_in_representation(parent, trail);
	}

	void insert_node(const PFNodeID& node, boost::optional<Modification&> modification)
	{
		m_nodes[node.layer()].insert(node.index());
		if(modification) modification->insert_node(node);
	}

	void redo_modification(const Modification& modification, int commandDepth)
	{
		const std::set<PFNodeID>& erased = modification.erased_nodes();
		const std::set<PFNodeID>& inserted = modification.inserted_nodes();
		for(std::set<PFNodeID>::const_iterator it=erased.begin(), iend=erased.end(); it!=iend; ++it) erase_node(*it, boost::none);
		for(std::set<PFNodeID>::const_iterator it=inserted.begin(), iend=inserted.end(); it!=iend; ++it) insert_node(*it, boost::none);
		m_listeners->modification_redone(modification, commandDepth);
	}

	Modification replace_with_selection_impl(const PartitionForestSelection_CPtr& selection, int commandDepth)
	{
		Modification modification;

		for(int i=0, size=static_cast<int>(m_nodes.size()); i<size; ++i)
		{
			for(Layer::const_iterator jt=m_nodes[i].begin(), jend=m_nodes[i].end(); jt!=jend; ++jt)
			{
				modification.erase_node(PFNodeID(i, *jt));
			}
			m_nodes[i].clear();
		}

		for(NodeConstIterator it=selection->nodes_cbegin(), iend=selection->nodes_cend(); it!=iend; ++it)
		{
			const PFNodeID& node = *it;
			modification.insert_node(node);
			m_nodes[node.layer()].insert(node.index());
		}

		m_listeners->selection_was_replaced(selection, commandDepth);
		return modification;
	}

	Modification select_node_impl(const PFNodeID& node, int commandDepth)
	{
		/*
		Cases:

		- The node is already part of the selection (do nothing)
		- An ancestor of the node is already part of the selection (do nothing)
		- One or more descendants of the node are already part of the selection (erase them and insert this instead)
		- Otherwise (insert the node as usual)
		*/

		// Note that this handles the first two cases.
		if(contains(node)) return Modification();

		Modification modification;

		// If there are any descendants, erase them.
		std::list<PFNodeID> descendants = descendants_in_representation(node);
		for(std::list<PFNodeID>::const_iterator it=descendants.begin(), iend=descendants.end(); it!=iend; ++it)
		{
			erase_node(*it, modification);
		}

		insert_node(node, modification);

		// Note: This is designed to work even if the node actually has no parent.
		consolidate_upwards_from_node(m_forest->parent_of(node), modification);

		m_listeners->node_was_selected(node, commandDepth);
		return modification;
	}

	void split_selection(std::stack<PFNodeID>& trail, Modification& modification)
	{
		while(!trail.empty())
		{
			PFNodeID cur = trail.top();
			trail.pop();
			deconsolidate_node(cur, modification);
		}
	}

	void undo_modification(const Modification& modification, int commandDepth)
	{
		const std::set<PFNodeID>& erased = modification.erased_nodes();
		const std::set<PFNodeID>& inserted = modification.inserted_nodes();
		for(std::set<PFNodeID>::const_iterator it=erased.begin(), iend=erased.end(); it!=iend; ++it) insert_node(*it, boost::none);
		for(std::set<PFNodeID>::const_iterator it=inserted.begin(), iend=inserted.end(); it!=iend; ++it) erase_node(*it, boost::none);
		m_listeners->modification_undone(modification, commandDepth);
	}
};

}

using mp_PartitionForestSelection::PartitionForestSelection;

}

#endif
