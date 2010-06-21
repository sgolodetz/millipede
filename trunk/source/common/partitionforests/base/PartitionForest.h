/***
 * millipede: PartitionForest.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFOREST
#define H_MILLIPEDE_PARTITIONFOREST

#include <climits>
#include <deque>
#include <queue>
#include <set>

#include <boost/bind.hpp>
#include <boost/optional.hpp>
#include <boost/tuple/tuple.hpp>

#include <common/commands/BasicCommandManager.h>
#include <common/commands/Command.h>
#include <common/commands/CommandSequenceGuard.h>
#include <common/exceptions/Exception.h>
#include <common/io/util/OSSWrapper.h>
#include <common/listeners/CompositeListenerBase.h>
#include "IForestLayer.h"
#include "PFNodeID.h"

namespace mp {

namespace mp_PartitionForest {

using namespace boost;

/**
@brief	A partition forest is a nested hierarchy of adjacency graphs that all partition the same aggregate object.
		For a more detailed and formal definition, please refer to my thesis.

This class template provides a generic implementation of partition forests that can be applied
in multiple contexts. Clients must provide their own leaf layer and branch layer implementations,
since the way in which these can be implemented efficiently varies with context.

@tparam	LeafLayerType		The type containing the client's implementation of a leaf layer
@tparam BranchLayerType		The type containing the client's implementation of a branch layer
*/
template <typename LeafLayerType, typename BranchLayerType>
class PartitionForest
{
	//#################### TYPEDEFS ####################
public:
	/// The type of branch layer used (exposing it to clients)
	typedef BranchLayerType BranchLayer;

	/// The type of leaf layer used (exposing it to clients)
	typedef LeafLayerType LeafLayer;

	/// The properties associated with a branch node
	typedef typename BranchLayer::NodeProperties BranchProperties;

	/// A chain of nodes produced during an unzip_node() operation
	typedef std::deque<PFNodeID> Chain;

	/// An edge in any of the layers (note that LeafLayer::Edge must equal BranchLayer::Edge)
	typedef typename LeafLayer::Edge Edge;

	/// The weight on an edge in any of the layers (note that LeafLayer::EdgeWeight must equal BranchLayer::EdgeWeight)
	typedef typename LeafLayer::EdgeWeight EdgeWeight;

	/// The properties associated with a leaf node
	typedef typename LeafLayer::NodeProperties LeafProperties;

protected:
	typedef shared_ptr<BranchLayer> BranchLayer_Ptr;
	typedef shared_ptr<ICommandManager> ICommandManager_Ptr;
	typedef IForestLayer<BranchProperties,EdgeWeight> IForestLayerT;
	typedef shared_ptr<IForestLayerT> IForestLayer_Ptr;
	typedef shared_ptr<LeafLayer> LeafLayer_Ptr;

public:
	/**
	@name Iterator Types
	*/
	//@{

	/// Iterator type that allows the nodes in a branch layer to be traversed in sequence as branch nodes (for reading only).
	typedef typename BranchLayer::BranchNodeConstIterator BranchNodeConstIterator;

	/// Iterator type that allows the nodes in the leaf layer to be traversed in sequence as leaf nodes (for reading only).
	typedef typename LeafLayer::LeafNodeConstIterator LeafNodeConstIterator;

	/// Iterator type that allows the edges in a layer to be traversed in sequence (for reading only).
	typedef typename IForestLayerT::EdgeConstIterator EdgeConstIterator;

	/// Iterator type that allows the nodes in a layer to be traversed in sequence as generic nodes (for reading only).
	typedef typename IForestLayerT::NodeConstIterator NodeConstIterator;

	//@}

	//#################### CONSTANTS ####################
public:
	enum CheckPreconditions
	{
		DONT_CHECK_PRECONDITIONS	= 0,
		CHECK_PRECONDITIONS			= 1,
	};

	//#################### NESTED CLASSES (EXCLUDING COMMANDS) ####################
public:
	class Listener
	{
	public:
		virtual ~Listener() {}

		virtual void forest_changed()																{}
		virtual void layer_was_cloned(int index)													{ forest_changed(); }
		virtual void layer_was_deleted(int index)													{ forest_changed(); }
		virtual void layer_was_undeleted(int index)													{ forest_changed(); }
		virtual void layer_will_be_deleted(int index)												{ forest_changed(); }
		virtual void node_was_split(const PFNodeID& node, const std::set<PFNodeID>& results)		{ forest_changed(); }
		virtual void nodes_were_merged(const std::set<PFNodeID>& nodes, const PFNodeID& result)		{ forest_changed(); }
		virtual void nodes_will_be_merged(const std::set<PFNodeID>& nodes)							{ forest_changed(); }
	};

private:
	class CompositeListener : public CompositeListenerBase<Listener>
	{
	public:
		void forest_changed()															{ multicast(bind(&Listener::forest_changed, _1)); }
		void layer_was_cloned(int index)												{ multicast(bind(&Listener::layer_was_cloned, _1, index)); }
		void layer_was_deleted(int index)												{ multicast(bind(&Listener::layer_was_deleted, _1, index)); }
		void layer_was_undeleted(int index)												{ multicast(bind(&Listener::layer_was_undeleted, _1, index)); }
		void layer_will_be_deleted(int index)											{ multicast(bind(&Listener::layer_will_be_deleted, _1, index)); }
		void node_was_split(const PFNodeID& node, const std::set<PFNodeID>& results)	{ multicast(bind(&Listener::node_was_split, _1, node, results)); }
		void nodes_were_merged(const std::set<PFNodeID>& nodes, const PFNodeID& result)	{ multicast(bind(&Listener::nodes_were_merged, _1, nodes, result)); }
		void nodes_will_be_merged(const std::set<PFNodeID>& nodes)						{ multicast(bind(&Listener::nodes_will_be_merged, _1, nodes)); }
	};

	//#################### COMMANDS ####################
private:
	struct CloneAboveLayerCommand : Command
	{
		PartitionForest *m_base;
		int m_indexB;

		CloneAboveLayerCommand(PartitionForest *base, int indexB)
		:	Command("Clone Above Layer"), m_base(base), m_indexB(indexB)
		{}

		void execute()		{ m_base->clone_layer_impl(m_indexB); }
		void undo()			{ m_base->delete_layer_impl(m_indexB + 1); }
	};

	struct DeleteLayerCommand : Command
	{
		PartitionForest *m_base;
		int m_indexD;
		BranchLayer_Ptr m_layerD;

		DeleteLayerCommand(PartitionForest *base, int indexD)
		:	Command("Delete Layer"), m_base(base), m_indexD(indexD)
		{}

		void execute()	{ m_layerD = m_base->delete_layer_impl(m_indexD); }
		void undo()		{ m_base->undelete_layer_impl(m_indexD, m_layerD); }
	};

	struct MergeSiblingNodesCommand : Command
	{
		PartitionForest *m_base;
		std::set<PFNodeID> m_nodes;

		std::vector<std::set<int> > m_splitGroups;
		optional<PFNodeID> m_result;

		MergeSiblingNodesCommand(PartitionForest *base, const std::set<PFNodeID>& nodes)
		:	Command("Merge Sibling Nodes"), m_base(base), m_nodes(nodes)
		{}

		void execute()
		{
			if(m_splitGroups.empty())
			{
				// Construct the split groups for a later undo operation.
				int layerIndex = m_nodes.begin()->layer();	// note that m_nodes is non-empty (see checks above)
				BranchLayer_Ptr layer = m_base->branch_layer(layerIndex);
				for(std::set<PFNodeID>::const_iterator it=m_nodes.begin(), iend=m_nodes.end(); it!=iend; ++it)
				{
					m_splitGroups.push_back(layer->node_children(it->index()));
				}
			}

			m_result = m_base->merge_sibling_nodes_impl(m_nodes);
		}

		const PFNodeID& result() const	{ return *m_result; }
		void undo()						{ m_base->split_node_impl(*m_result, m_splitGroups); }
	};

	struct SplitNodeCommand : Command
	{
		PartitionForest *m_base;
		PFNodeID m_node;
		std::vector<std::set<int> > m_groups;
		std::set<PFNodeID> m_result;

		SplitNodeCommand(PartitionForest *base, const PFNodeID& node, const std::vector<std::set<int> >& groups)
		:	Command("Split Node"), m_base(base), m_node(node), m_groups(groups)
		{}

		void execute()								{ m_result = m_base->split_node_impl(m_node, m_groups); }
		const std::set<PFNodeID>& result() const	{ return m_result; }
		void undo()									{ m_base->merge_sibling_nodes_impl(m_result); }
	};

	//#################### PRIVATE VARIABLES ####################
private:
	LeafLayer_Ptr m_leafLayer;						// the partitioning graph for the leaf layer
	std::vector<BranchLayer_Ptr> m_branchLayers;	// the partitioning graphs for the branch layers
	ICommandManager_Ptr m_commandManager;
	CompositeListener m_listeners;

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Constructs an initial partition forest from a leaf layer (and, optionally, a corresponding
			lowest branch layer).

	The facility to generate a lowest branch layer externally and then supply it as an argument here is	provided
	because it may be prohibitively inefficient to generate it by cloning the leaf layer and then merging nodes
	(this would be the only option if we forced clients to construct it in situ).

	@param[in]	leafLayer			A shared_ptr (non-null) to the leaf layer
	@param[in]	lowestBranchLayer	An optional shared_ptr (possibly null) to the lowest branch layer
	@pre
		-	leafLayer.get() != NULL
		-	The layers pointed to by leafLayer and lowestBranchLayer properly correspond to each other
			(in the obvious manner)
	*/
	explicit PartitionForest(const LeafLayer_Ptr& leafLayer, const BranchLayer_Ptr& lowestBranchLayer = BranchLayer_Ptr())
	:	m_leafLayer(leafLayer), m_commandManager(new BasicCommandManager)
	{
		if(lowestBranchLayer) m_branchLayers.push_back(lowestBranchLayer);
	}

	//#################### DESTRUCTOR ####################
public:
	/**
	@brief	The destructor is virtual because other classes may want to derive from PartitionForest.
	*/
	virtual ~PartitionForest()
	{}

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
private:
	/**
	@brief	The default compiler-generated copy constructor would be dangerous: this disables it.
	*/
	PartitionForest(const PartitionForest&);

	/**
	@brief	The default compiler-generated assignment operator would be dangerous: this disables it.
	*/
	PartitionForest& operator=(const PartitionForest&);

	//#################### PUBLIC METHODS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/**
	@name	Auxiliary Methods
	*/
	//@{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	@brief	Adds a weak listener to be notified of changes to the partition forest.

	A weak listener is one to which a handle (in particular, a shared_ptr) must be externally maintained.
	If there are no remaining external references to the listener, it ceases to exist (and is therefore
	no longer notified of any changes).

	@param[in]	listener	A weak_ptr (non-null) to the listener
	@pre
		-	listener.get() != NULL
	@post
		-	The argument is registered as listening to this partition forest (and will be alerted to any changes)
	*/
	void add_weak_listener(const weak_ptr<Listener>& listener)
	{
		m_listeners.add_weak_listener(listener);
	}

	/**
	@brief	Provides a general way of constructing a lowest branch layer from a leaf layer and a specified
			partitioning of the leaf nodes it contains.

	The lowest branch layer constructed will contain a node for each group. Each such node will contain child
	links to the leaf nodes in its group. Corresponding parent links will be added to the nodes in the leaf
	layer (this is why leafLayer is marked as an in/out parameter - although it isn't modified itself, the
	leaf layer is changed through it).

	@param[in,out]	leafLayer	The leaf layer
	@param[in]		groups		The groups into which to partition the leaf nodes
	@pre
		-	The specified groups must form a partition of the leaf nodes
	@throw Exception
		-	If any of the specified groups are empty
	@return A shared_ptr to the newly-constructed lowest branch layer
	*/
	static BranchLayer_Ptr make_lowest_branch_layer(const LeafLayer_Ptr& leafLayer, const std::vector<std::set<int> >& groups)
	{
		BranchLayer_Ptr lowestBranchLayer(new BranchLayer);

		// Add the forest links and calculate the lowest branch layer node properties.
		for(size_t i=0, size=groups.size(); i<size; ++i)
		{
			const std::set<int>& group = groups[i];
			if(group.empty()) throw Exception("Empty branch group");
			int parentIndex = *group.begin();

			for(std::set<int>::const_iterator jt=group.begin(), jend=group.end(); jt!=jend; ++jt)
			{
				leafLayer->set_node_parent(*jt, parentIndex);
			}

			lowestBranchLayer->set_node_children(parentIndex, group);
			lowestBranchLayer->set_node_properties(parentIndex, leafLayer->combine_properties(group));
		}

		// Add the edges between adjacent lowest branch layer nodes.
		for(typename LeafLayer::EdgeConstIterator it=leafLayer->edges_cbegin(), iend=leafLayer->edges_cend(); it!=iend; ++it)
		{
			Edge e = *it;
			int parentU = leafLayer->node_parent(e.u);
			int parentV = leafLayer->node_parent(e.v);
			if(parentU != parentV)
			{
				lowestBranchLayer->update_edge_weight(parentU, parentV, e.weight);
			}
		}

		return lowestBranchLayer;
	}

	/**
	@brief	Outputs a textual representation of the partition forest to a std::ostream.

	@param[out]	os	A reference to the std::ostream to which to output the partition forest
	*/
	void output(std::ostream& os) const
	{
		os << "LAYER 0\n\n";

		std::vector<int> leafNodes = m_leafLayer->node_indices();
		os << "Nodes:\n";
		for(int i=0, size=static_cast<int>(leafNodes.size()); i<size; ++i)
		{
			os << "\n[" << leafNodes[i] << ", " << m_leafLayer->node_properties(leafNodes[i]) << ", " << m_leafLayer->node_parent(leafNodes[i]) << "] ";
		}
		os << "\n\n";

		os << "Edges:\n";
		for(typename LeafLayer::EdgeConstIterator it=m_leafLayer->edges_cbegin(), iend=m_leafLayer->edges_cend(); it!=iend; ++it)
		{
			os << '\n' << *it;
		}
		os << "\n\n";

		for(int layer=1, highestLayer=highest_layer(); layer<=highestLayer; ++layer)
		{
			os << "LAYER " << layer << "\n\n";

			std::vector<int> branchNodes = m_branchLayers[layer-1]->node_indices();
			os << "Nodes:\n";
			for(int i=0, size=static_cast<int>(branchNodes.size()); i<size; ++i)
			{
				os	<< "\n["
					<< branchNodes[i] << ", "
					<< m_branchLayers[layer-1]->node_properties(branchNodes[i]) << ", "
					<< "{ ";

				std::set<int> children = m_branchLayers[layer-1]->node_children(branchNodes[i]);
				std::copy(children.begin(), children.end(), std::ostream_iterator<int>(os, " "));

				os	<< "}, "
					<< m_branchLayers[layer-1]->node_parent(branchNodes[i])
					<< "] ";
			}
			os << "\n\n";

			std::vector<Edge> branchEdges = m_branchLayers[layer-1]->edges();
			os << "Edges:\n";
			for(int i=0, size=static_cast<int>(branchEdges.size()); i<size; ++i)
			{
				os << '\n' << branchEdges[i];
			}
			os << "\n\n";
		}
	}

	/**
	@brief	Sets the command manager that the partition forest should use.

	By default, a partition forest uses a BasicCommandManager to manage command execution.
	This is a simple command manager that executes commands without storing them, meaning
	that once they're executed, they can't be undone. It is ideal when constructing the
	forest, or when undo facilities are unimportant (or the overhead cannot be afforded).
	If and when undo facilities are required, however, the command manager can be replaced
	with an UndoableCommandManager (or a program's own command manager, by deriving from
	ICommandManager).

	@param[in]	commandManager	A shared_ptr to the command manager to use
	@post
		-	The partition forest's command manager is replaced with the argument
	*/
	void set_command_manager(const ICommandManager_Ptr& commandManager)
	{
		m_commandManager = commandManager;
	}

	//@}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/**
	@name	Graph Accessors
	*/
	//@{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	@brief	Returns the edges adjacent to the specified forest node in its layer.

	@param[in]	node	The ID of the node
	@pre
		-	0 <= node.layer() <= highest_layer()
	@throw Exception
		-	If the precondition is violated
		-	If the specified ID does not refer to a valid node
	@return	As described
	*/
	std::vector<Edge> adjacent_edges(const PFNodeID& node) const
	{
		IForestLayer_Ptr layer = checked_forest_layer(node.layer());
		if(!layer) throw Exception(OSSWrapper() << "Invalid layer: " << node.layer());
		return layer->adjacent_edges(node.index());
	}

	/**
	@brief	Returns the indices of the nodes adjacent to the specified forest node in its layer.

	The indices are merely the index components of the adjacent nodes' IDs.

	@param[in]	node	The ID of the node
	@pre
		-	0 <= node.layer() <= highest_layer()
	@throw Exception
		-	If the precondition is violated
		-	If the specified ID does not refer to a valid node
	@return	As described
	*/
	std::vector<int> adjacent_nodes(const PFNodeID& node) const
	{
		IForestLayer_Ptr layer = checked_forest_layer(node.layer());
		if(!layer) throw Exception(OSSWrapper() << "Invalid layer: " << node.layer());
		return layer->adjacent_nodes(node.index());
	}

	//@}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/**
	@name	Hierarchy Accessors
	*/
	//@{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	@brief Finds the ancestor of a node in the specified layer.

	@param[in]	node		The ID of the node
	@param[in]	layerIndex	The layer in which to find the relevant ancestor of the node
	@pre
		-	node.layer() <= layerIndex <= highest_layer()
	@throw Exception
		-	If the precondition is violated
		-	If the specified ID does not refer to a valid node
	@return The ancestor of the node in the specified layer
	*/
	PFNodeID ancestor_of(const PFNodeID& node, int layerIndex) const
	{
		if(layerIndex < node.layer() || layerIndex > highest_layer())
		{
			throw Exception(OSSWrapper() << "Invalid layer: " << layerIndex);
		}

		PFNodeID cur = node;
		while(cur.layer() < layerIndex)
		{
			IForestLayer_Ptr layer = forest_layer(cur.layer());
			cur = PFNodeID(cur.layer() + 1, layer->node_parent(cur.index()));
		}
		return cur;
	}

	/**
	@brief	Returns the node IDs of the children of the specified node.

	If the node is in the leaf layer, it has no children, so the empty set is returned.

	@param[in]	node	The ID of the node
	@throw Exception
		-	If the specified ID does not refer to a valid node
	@return
		-	The node's children, if the node is a branch node
		-	The empty set, if the node is a leaf node
	*/
	std::set<PFNodeID> children_of(const PFNodeID& node) const
	{
		// Special case: nodes in the leaf layer have no children.
		if(node.layer() == 0) return std::set<PFNodeID>();

		BranchLayer_Ptr layer = checked_branch_layer(node.layer());
		if(!layer) throw Exception(OSSWrapper() << "Invalid layer: " << node.layer());
		if(!layer->has_node(node.index())) throw Exception(OSSWrapper() << "Invalid node: " << node);

		std::set<PFNodeID> ret;

		std::set<int> children = layer->node_children(node.index());
		int childLayerIndex = node.layer() - 1;
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			ret.insert(PFNodeID(childLayerIndex, *it));
		}

		return ret;
	}

	/**
	@brief	Returns the index of the highest layer in the partition forest.

	@return	The index of the highest layer, as described
	*/
	int highest_layer() const
	{
		return static_cast<int>(m_branchLayers.size());
	}

	/**
	@brief	Returns the node ID of the parent of the specified node (if any).

	@param[in]	node	The ID of the node
	@throw Exception
		-	If the specified ID does not refer to a valid node
	@return
		-	The ID of the node's parent, if node.layer() != highest_layer()
		-	PFNodeID::invalid(), otherwise
	*/
	PFNodeID parent_of(const PFNodeID& node) const
	{
		IForestLayer_Ptr layer = checked_forest_layer(node.layer());
		if(layer && layer->has_node(node.index()))
		{
			int parentIndex = layer->node_parent(node.index());
			if(parentIndex != -1) return PFNodeID(node.layer() + 1, parentIndex);
			else return PFNodeID::invalid();
		}
		else throw Exception(OSSWrapper() << "Invalid node: " << node);
	}

	/**
	@brief	Determines the <em>receptive region</em> of the specified node.

	@note	The receptive region of a node is the set of its leaf node descendants in the partition forest.

	@param[in]	node	The ID of the node
	@throw Exception
		-	If the specified ID does not refer to a valid node
	@return A list of the indices of the leaf nodes in the receptive region of the node
	*/
	std::list<int> receptive_region_of(const PFNodeID& node) const
	{
		// Note: This is not currently optimized -- it's mostly for debugging/output purposes right now.
		IForestLayer_Ptr layer = checked_forest_layer(node.layer());
		if(!layer || !layer->has_node(node.index())) throw Exception(OSSWrapper() << "Invalid node: " << node);

		std::list<int> result;

		std::list<PFNodeID> q;
		q.push_back(node);
		while(!q.empty())
		{
			PFNodeID n = q.front();
			q.pop_front();

			if(n.layer() != 0)
			{
				std::set<PFNodeID> children = children_of(n);
				std::copy(children.begin(), children.end(), std::back_inserter(q));
			}
			else result.push_back(n.index());
		}

		return result;
	}

	//@}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/**
	@name	Node Property Accessors
	*/
	//@{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	@brief	Returns the properties associated with the specified branch node.

	@param[in]	node	The ID of the node
	@throw Exception
		-	If the specified ID does not refer to a valid node
	@return The properties of the branch node, as described
	*/
	const BranchProperties& branch_properties(const PFNodeID& node) const
	{
		BranchLayer_Ptr branchLayer = checked_branch_layer(node.layer());
		if(branchLayer && branchLayer->has_node(node.index())) return branchLayer->node_properties(node.index());
		else throw Exception(OSSWrapper() << "Invalid node: " << node);
	}

	/**
	@brief	Returns the properties associated with the specified leaf node.

	@note	Since all leaf nodes are in layer 0, only the index of the leaf node
			is passed in, rather than a full node ID.

	@param[in]	n	The index of the leaf node
	@throw Exception
		-	If the specified ID does not refer to a valid node
	@return The properties of the leaf node, as described
	*/
	const LeafProperties& leaf_properties(int n) const
	{
		if(m_leafLayer->has_node(n)) return m_leafLayer->node_properties(n);
		else throw Exception(OSSWrapper() << "Invalid leaf: " << n);
	}

	//@}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/**
	@name	Iterators
	*/
	//@{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	@brief	Returns a BranchNodeConstIterator referring to the start of the branch nodes in the specified layer.

	In conjunction with branch_nodes_cend(), this allows the nodes in a given branch layer to be accessed in
	sequence as branch nodes. This is in contrast to the more generic interface provided by nodes_cbegin() and
	nodes_cend(), which only allow the same nodes to be accessed as generic nodes.

	@note	It is implicit that corresponding calls to branch_nodes_cbegin() and branch_nodes_cend() must specify
			the same branch layer index. If the layer indices differ, iterator comparisons between the two results
			will be undefined.

	@param[in]	layerIndex	The index of the branch layer
	@pre
		-	1 <= layerIndex <= highest_layer()
	@throw Exception
		-	If the precondition is violated
	@return The BranchNodeConstIterator, as described
	*/
	BranchNodeConstIterator branch_nodes_cbegin(int layerIndex) const
	{
		BranchLayer_Ptr layer = checked_branch_layer(layerIndex);
		if(layer) return layer->branch_nodes_cbegin();
		else throw Exception(OSSWrapper() << "Invalid layer: " << layerIndex);
	}

	/**
	@brief	Returns a BranchNodeConstIterator referring to the end of the branch nodes in the specified layer.

	@see branch_nodes_cbegin

	@param[in]	layerIndex	The index of the branch layer
	@pre
		-	1 <= layerIndex <= highest_layer()
	@throw Exception
		-	If the precondition is violated
	@return The BranchNodeConstIterator, as described
	*/
	BranchNodeConstIterator branch_nodes_cend(int layerIndex) const
	{
		BranchLayer_Ptr layer = checked_branch_layer(layerIndex);
		if(layer) return layer->branch_nodes_cend();
		else throw Exception(OSSWrapper() << "Invalid layer: " << layerIndex);
	}

	/**
	@brief	Returns an EdgeConstIterator to the start of the edges in the specified layer.

	In conjunction with edges_cend(), this allows the edges in a given layer to be accessed in sequence.

	@note	It is implicit that corresponding calls to edges_cbegin() and edges_cend() must specify
			the same layer index. If the layer indices differ, iterator comparisons between the two
			results will be undefined.

	@param[in]	layerIndex	The index of the layer
	@pre
		-	0 <= layerIndex <= highest_layer()
	@throws Exception
		-	If the precondition is violated
	@return The EdgeConstIterator, as described
	*/
	EdgeConstIterator edges_cbegin(int layerIndex) const
	{
		IForestLayer_Ptr layer = checked_forest_layer(layerIndex);
		if(layer) return layer->edges_cbegin();
		else throw Exception(OSSWrapper() << "Invalid layer: " << layerIndex);
	}

	/**
	@brief	Returns an EdgeConstIterator to the end of the edges in the specified layer.

	@see edges_cbegin

	@param[in]	layerIndex	The index of the layer
	@pre
		-	0 <= layerIndex <= highest_layer()
	@throws Exception
		-	If the precondition is violated
	@return The EdgeConstIterator, as described
	*/
	EdgeConstIterator edges_cend(int layerIndex) const
	{
		IForestLayer_Ptr layer = checked_forest_layer(layerIndex);
		if(layer) return layer->edges_cend();
		else throw Exception(OSSWrapper() << "Invalid layer: " << layerIndex);
	}

	/**
	@brief	Returns a LeafNodeConstIterator referring to the start of the leaf nodes.

	In conjunction with leaf_nodes_cend(), this allows the nodes in the leaf layer to be accessed
	in sequence. This is in contrast to the more generic interface provided by nodes_cbegin() and
	nodes_cend(), which only allow the same nodes to be accessed as generic nodes.

	@return The LeafNodeConstIterator, as described
	*/
	LeafNodeConstIterator leaf_nodes_cbegin() const
	{
		return m_leafLayer->leaf_nodes_cbegin();
	}

	/**
	@brief	Returns a LeafNodeConstIterator referring to the end of the leaf nodes.

	@see leaf_nodes_cbegin

	@return The LeafNodeConstIterator, as described
	*/
	LeafNodeConstIterator leaf_nodes_cend() const
	{
		return m_leafLayer->leaf_nodes_cend();
	}

	/**
	@brief	Returns a NodeConstIterator referring to the start of the nodes in the specified layer.

	In conjunction with nodes_cend(), this allows the nodes in a given layer to be accessed in sequence
	as generic nodes.

	@note	It is implicit that corresponding calls to nodes_cbegin() and nodes_cend() must specify the
			same layer index. If the layer indices differ, iterator comparisons between the two results
			will be undefined.

	@param[in]	layerIndex	The index of the layer
	@pre
		-	0 <= layerIndex <= highest_layer()
	@throw Exception
		-	If the precondition is violated
	@return The NodeConstIterator, as described
	*/
	NodeConstIterator nodes_cbegin(int layerIndex) const
	{
		IForestLayer_Ptr layer = checked_forest_layer(layerIndex);
		if(layer) return layer->nodes_cbegin();
		else throw Exception(OSSWrapper() << "Invalid layer: " << layerIndex);
	}

	/**
	@brief	Returns a NodeConstIterator referring to the end of the nodes in the specified layer.

	@see nodes_cbegin

	@return The NodeConstIterator, as described
	*/
	NodeConstIterator nodes_cend(int layerIndex) const
	{
		IForestLayer_Ptr layer = checked_forest_layer(layerIndex);
		if(layer) return layer->nodes_cend();
		else throw Exception(OSSWrapper() << "Invalid layer: " << layerIndex);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/**
	@name	Core Mutators
	*/
	//@{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	@brief	Inserts a clone of the specified layer into the partition forest (above the clonee layer).

	@note	This method executes a CloneAboveLayerCommand (which can be undone, if an UndoableCommandManager
			has been previously installed using set_command_manager()).

	@param[in]	indexB				The index of the clonee layer
	@param[in]	checkPreconditions	Whether or not the preconditions need to be explicitly checked (default: yes)
	@pre
		-	0 <= indexB <= highest_layer()
	@post
		-	A clone of the specified layer will have been inserted into the partition forest as described
		-	Listeners will have been alerted that this has happened
	*/
	void clone_layer(int indexB, CheckPreconditions checkPreconditions = CHECK_PRECONDITIONS)
	{
		if(checkPreconditions)
		{
			if(indexB < 0 || indexB > highest_layer())
			{
				throw Exception(OSSWrapper() << "Invalid layer: " << indexB);
			}
		}

		m_commandManager->execute(Command_Ptr(new CloneAboveLayerCommand(this, indexB)));
	}

	/**
	@brief	Deletes the specified (branch) layer from the partition forest.

	The leaf layer cannot be deleted, as it is essential to know the primitive sub-objects in the aggregate.

	@note	This method executes a DeleteLayerCommand (which can be undone, if an UndoableCommandManager
			has been previously installed using set_command_manager()).

	@param[in]	indexD				The index of the layer to be deleted
	@param[in]	checkPreconditions	Whether or not the preconditions need to be explicitly checked (default: yes)
	@pre
		-	1 <= indexD <= highest_layer()
	@post
		-	The specified layer will have been deleted from the partition forest
		-	Listeners will have been alerted that this has happened
	*/
	void delete_layer(int indexD, CheckPreconditions checkPreconditions = CHECK_PRECONDITIONS)
	{
		if(checkPreconditions)
		{
			if(indexD < 1 || indexD > highest_layer())
			{
				throw Exception(OSSWrapper() << "Invalid layer: " << indexD);
			}
		}

		m_commandManager->execute(Command_Ptr(new DeleteLayerCommand(this, indexD)));
	}

	/**
	@brief	Merges a set of sibling nodes in the partition forest.

	@note	This method executes a MergeSiblingNodesCommand (which can be undone, if an UndoableCommandManager
			has been previously installed using set_command_manager()).

	@param[in]	nodes	The set of nodes to be merged
	@param[in]	checkPreconditions	Whether or not the preconditions need to be explicitly checked (default: yes)
	@pre
		-	!nodes.empty()
		-	The nodes are all valid, and share a common parent (they must thus all be in the same layer)
		-	The layer in which the nodes reside is not the lowest layer
		-	The union of the objects represented by the nodes to be merged is connected
	@post
		-	The partition forest will have been consistently modified so as to merge the specified nodes
		-	Listeners will have been alerted that this has happened
	@throw Exception
		-	If the preconditions are violated
	@return The ID of the node resulting from the merge
	*/
	PFNodeID merge_sibling_nodes(const std::set<PFNodeID>& nodes, CheckPreconditions checkPreconditions = CHECK_PRECONDITIONS)
	{
		if(checkPreconditions)
		{
			// Check that there are nodes to be merged.
			if(nodes.empty()) throw Exception("No nodes to merge");
			
			// Check that the nodes are not in the lowest layer of the forest and that they have a common parent. (Note that checking
			// their parents also implicitly checks whether the nodes themselves are valid.) Checking the parent of the first node
			// against itself is redundant, but it makes the code simpler and clearer.
			std::set<int> nodeIndices;
			PFNodeID commonParent = parent_of(*nodes.begin());
			for(std::set<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
			{
				if(it->layer() == 0) throw Exception("Cannot change lowest layer");
				if(parent_of(*it) != commonParent) throw Exception("The nodes are not siblings");
				nodeIndices.insert(it->index());
			}

			// Check that the union of the objects represented by the nodes to be merged is connected.
			if(!are_connected(nodeIndices, nodes.begin()->layer())) throw Exception("The merged node would not be connected");
		}

		shared_ptr<MergeSiblingNodesCommand> command(new MergeSiblingNodesCommand(this, nodes));
		m_commandManager->execute(command);
		return command->result();
	}

	/**
	@brief	Splits a node in the partition forest into a number of non-overlapping, connected pieces.

	The pieces are specified as groups of the node's children. For instance, if a node has the child set {0,3,4,6},
	we might e.g. pass in (subject to connectivity constraints) the groups [{0,4},{3,6}] to split the node into two.
	The node would then be replaced with two nodes, one with child set {0,4}, and the other with child set {3,6}.

	@note	This method executes a SplitNodeCommand (which can be undone, if an UndoableCommandManager
			has been previously installed using set_command_manager()).

	@param[in]	node	The node to be split
	@param[in]	groups	A partition of the node's children into groups
	@param[in]	checkPreconditions	Whether or not the preconditions need to be explicitly checked (default: yes)
	@pre
		-	node.layer() != 0
		-	The node to be split is valid
		-	The set of groups forms a partition of the node's children
		-	Each group is itself non-empty and connected
	@post
		-	The partition forest will have been consistently modified so as to replace the node being split with
			a node for each split group
		-	Listeners will have been alerted that this has happened
	@throw Exception
		-	If the preconditions are violated
	@return The set of nodes created as a result of the split
	*/
	std::set<PFNodeID> split_node(const PFNodeID& node, const std::vector<std::set<int> >& groups, CheckPreconditions checkPreconditions = CHECK_PRECONDITIONS)
	{
		if(checkPreconditions)
		{
			// Check that the node to be split is not in the lowest layer of the hierarchy.
			if(node.layer() == 0) throw Exception("Cannot change lowest layer");

			// Check that the node to be split is valid.
			BranchLayer_Ptr layer = checked_branch_layer(node.layer());	// the layer in which the node is being split
			if(!layer || !layer->has_node(node.index())) throw Exception(OSSWrapper() << "Invalid node: " << node);

			// Check that the groups partition the children of the node being split.
			std::set<int> children = layer->node_children(node.index());
			for(std::vector<std::set<int> >::const_iterator it=groups.begin(), iend=groups.end(); it!=iend; ++it)
			{
				for(std::set<int>::const_iterator jt=it->begin(), jend=it->end(); jt!=jend; ++jt)
				{
					std::set<int>::iterator kt = children.find(*jt);
					if(kt != children.end()) children.erase(kt);
					else throw Exception("The groups do not partition the children of the node to be split");
				}
			}
			if(!children.empty()) throw Exception("The groups do not partition the children of the node to be split");

			// Check that each of the split groups is non-empty and connected.
			for(std::vector<std::set<int> >::const_iterator it=groups.begin(), iend=groups.end(); it!=iend; ++it)
			{
				if(it->empty()) throw Exception("One of the split groups is empty");
				if(!are_connected(*it, node.layer() - 1)) throw Exception("One of the split groups is not connected");
			}
		}

		shared_ptr<SplitNodeCommand> command(new SplitNodeCommand(this, node, groups));
		m_commandManager->execute(command);
		return command->result();
	}

	//@}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/**
	@name	Zipping Mutators
	*/
	//@{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	@brief	Unzips a node in the partition forest as far up as the specified layer.

	An unzip node operation is essentially a multi-layer split. Each ancestor of
	the specified node in the layers [node.layer()+1, toLayer] is split into pieces.
	For each split, one of those pieces has the same receptive region as the node being
	unzipped, whilst the other pieces are determined to be the connected components of
	what remains of the split node.

	@note	This method executes a command sequence (which can be atomically undone, if an UndoableCommandManager
			has been previously installed using set_command_manager()). Listeners will be alerted to the individual
			changes involved, rather than the execution of the sequence as a whole.

	@param[in]	node				The node to be unzipped
	@param[in]	toLayer				The layer to which to unzip it
	@param[in]	checkPreconditions	Whether or not the preconditions need to be explicitly checked (default: yes)
	@pre
		-	The specified node is valid
		-	node.layer() <= toLayer <= highest_layer()
	@post
		-	The partition forest will have been consistently modified so as to unzip the node to the specified layer
	@throw Exception
		-	If the preconditions are violated
	@return The node chains generated by the unzip (see my thesis for details, and also see zip_chains())
	*/
	std::vector<Chain> unzip_node(const PFNodeID& node, int toLayer = highest_layer(), CheckPreconditions checkPreconditions = CHECK_PRECONDITIONS)
	{
		if(checkPreconditions)
		{
			// Check that the specified node is valid.
			IForestLayer_Ptr layer = checked_forest_layer(node.layer());
			if(!layer || !layer->has_node(node.index())) throw Exception(OSSWrapper() << "Invalid node: " << node);

			// Check that the specified layer is within the right range.
			if(toLayer < node.layer() || toLayer > highest_layer()) throw Exception(OSSWrapper() << "Invalid layer: " << toLayer);
		}

		CommandSequenceGuard guard(m_commandManager, "Unzip Node");

		std::vector<Chain> chains;

		// Ensure that the chain leading up from the node being unzipped is the first chain in the returned vector.
		// The chain shouldn't actually contain the node itself -- we remove it before returning.
		Chain primaryChain;
		primaryChain.push_back(node);
		chains.push_back(primaryChain);

		PFNodeID cur = node;
		while(cur.layer() < toLayer)
		{
			PFNodeID parent = parent_of(cur);
			BranchLayer_Ptr parentLayer = branch_layer(parent.layer());

			// Let siblings = children(parent) \ {cur}.
			std::set<int> siblings = parentLayer->node_children(parent.index());
			siblings.erase(cur.index());

			// Calculate the connected components of the siblings.
			std::vector<std::set<int> > connectedComponents = find_connected_components(siblings, cur.layer());

			// Add in the component {cur} and split the parent node.
			std::set<int> curComponent;
			curComponent.insert(cur.index());
			connectedComponents.push_back(curComponent);
			std::set<PFNodeID> result = split_node(parent, connectedComponents, DONT_CHECK_PRECONDITIONS);

			// Update the chains.
			for(std::vector<Chain>::iterator it=chains.begin(), iend=chains.end(); it!=iend; ++it)
			{
				PFNodeID h = it->front();	// each chain is non-empty by construction
				PFNodeID p = parent_of(h);
				it->push_front(p);
				result.erase(p);
			}

			for(std::set<PFNodeID>::const_iterator it=result.begin(), iend=result.end(); it!=iend; ++it)
			{
				Chain chain;
				chain.push_back(*it);
				chains.push_back(chain);
			}

			cur = parent_of(cur);	// note: not necessarily the same as the parent before split_node() was invoked!
		}

		// The first chain in the vector incorrectly contains the node being unzipped as its last node at this point, so we remove it.
		chains.front().pop_back();

		return chains;
	}

	/**
	@brief	Zips chains of sibling nodes in the partition forest together.

	A zip chains operation is essentially a multi-layer sibling node merge. The chains (which do not all have
	to be of the same length, but must share the same highest layer) are lined up next to each other and the
	corresponding sibling nodes in each layer are merged, starting from the highest layer and working downwards.
	For example, if we were merging the hypothetical chains [(3,0),(2,0),(1,5)], [(3,3),(2,4)] and [(3,2),(2,2)],
	then the sequence of sibling merges would be {(3,0),(3,3),(3,2)}, {(2,0),(2,4),(2,2)} and (trivially) {(1,5)}.

	@note	This method executes a command sequence (which can be atomically undone, if an UndoableCommandManager
			has been previously installed using set_command_manager()). Listeners will be alerted to the individual
			changes involved, rather than the execution of the sequence as a whole.

	@param[in]	chains				The chains of nodes
	@param[in]	checkPreconditions	Whether or not the preconditions need to be explicitly checked (default: yes)
	@pre
		-	!chains.empty()
		-	Every chain node is valid
		-	Each chain is non-empty, and does not extend down as far as the leaf layer (which is immutable)
		-	The highest nodes in the chains are siblings of each other
		-	The sets of nodes to be merged in each layer are connected
	@post
		-	The partition forest will have been consistently modified so as to zip the chains together
	@throw Exception
		-	If the preconditions are violated
	@return A pair, the first component of which is the node resulting from the final merge, and the second
			component of which is the layer at which the chains started
	*/
	std::pair<PFNodeID,int> zip_chains(const std::vector<Chain>& chains, CheckPreconditions checkPreconditions = CHECK_PRECONDITIONS)
	{
		if(checkPreconditions)
		{
			// Check that there are chains to zip.
			if(chains.empty()) throw Exception("No chains to zip");

			// TODO: It might be sensible to program defensively and check that every chain node is actually valid.

			// Check that each chain is non-empty and doesn't extend down as far as the bottom layer of the hierarchy.
			int lowLayer = INT_MAX;
			for(std::vector<Chain>::const_iterator it=chains.begin(), iend=chains.end(); it!=iend; ++it)
			{
				if(it->empty()) throw Exception("One of the chains is empty");
				if(it->back().layer() == 0) throw Exception("Cannot change lowest layer");
				lowLayer = std::min(lowLayer, it->back().layer());
			}

			// Check that the highest nodes in the chains are siblings of each other.
			PFNodeID commonParent = parent_of(chains[0].front());	// note: there is guaranteed to be a non-empty chain 0 (see earlier checks)
			for(std::vector<Chain>::const_iterator it=chains.begin() + 1, iend=chains.end(); it!=iend; ++it)
			{
				if(parent_of(it->front()) != commonParent) throw Exception("The highest chain nodes are not siblings");
			}

			// Check that the sets of nodes to be merged in each layer are connected.
			int highLayer = chains[0].front().layer();
			for(int i=highLayer; i>=lowLayer; --i)
			{
				std::set<int> nodes;
				for(std::vector<Chain>::const_iterator jt=chains.begin(), jend=chains.end(); jt!=jend; ++jt)
				{
					const Chain& chain = *jt;
					if(highLayer - i < static_cast<int>(chain.size()))
					{
						nodes.insert(chain[highLayer - i].index());
					}
				}
				if(!are_connected(nodes, i)) throw Exception(OSSWrapper() << "The chain nodes in layer " << i << " are not connected");
			}
		}

		CommandSequenceGuard guard(m_commandManager, "Zip Chains");

		// Find the high and low layers of the chains.
		int highLayer = chains[0].front().layer();
		int lowLayer = INT_MAX;
		for(std::vector<Chain>::const_iterator it=chains.begin(), iend=chains.end(); it!=iend; ++it)
		{
			lowLayer = std::min(lowLayer, it->back().layer());
		}

		// Perform a sibling node merge on the nodes in each layer, starting from the high layer and working downwards.
		PFNodeID mergeResult = PFNodeID::invalid();
		for(int i=highLayer; i>=lowLayer; --i)
		{
			std::set<PFNodeID> nodes;
			for(std::vector<Chain>::const_iterator jt=chains.begin(), jend=chains.end(); jt!=jend; ++jt)
			{
				const Chain& chain = *jt;
				if(highLayer - i < static_cast<int>(chain.size()))
				{
					nodes.insert(PFNodeID(i, chain[highLayer - i].index()));
				}
			}
			mergeResult = merge_sibling_nodes(nodes, DONT_CHECK_PRECONDITIONS);
		}

		return std::make_pair(mergeResult, highLayer);
	}

	//@}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/**
	@name	Higher-Level Mutators
	*/
	//@{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	@brief	Merges a set of nodes in the same layer of the partition forest into their connected components.

	This a more general merging algorithm than merge_sibling_nodes(). It weakens the preconditions required
	to merge nodes to the minimum possible. Given a set of nodes to be merged (which must all be in the same
	layer), the algorithm first determines their connected components using the adjacency graph for their layer,
	and then merges the nodes in each component using zipping operations.

	@note	This method executes a command sequence (which can be atomically undone, if an UndoableCommandManager
			has been previously installed using set_command_manager()). Listeners will be alerted to the individual
			changes involved, rather than the execution of the sequence as a whole.

	@param[in]	nodes				The nodes to be merged
	@param[in]	checkPreconditions	Whether or not the preconditions need to be explicitly checked (default: yes)
	@pre
		-	!nodes.empty()
		-	Each of the nodes to be merged is valid
		-	All of the nodes are in the same, non-leaf, layer
	@post
		-	The partition forest will have been consistently modified so as to merge the specified nodes into their
			connected components in their layer's adjacency graph
	@throw Exception
		-	If the preconditions are violated
	@return A set of nodes, each of which is the result of merging the input nodes in one connected component
	*/
	std::set<PFNodeID> merge_nonsibling_nodes(const std::set<PFNodeID>& nodes, CheckPreconditions checkPreconditions = CHECK_PRECONDITIONS)
	{
		if(checkPreconditions)
		{
			// Check that there are nodes to be merged.
			if(nodes.empty()) throw Exception("No nodes to merge");

			// Check that the nodes to be merged are valid and are all in the same (non-lowest) layer of the hierarchy.
			int commonLayer = nodes.begin()->layer();
			if(commonLayer == 0) throw Exception("Cannot change lowest layer");
			for(std::set<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
			{
				IForestLayer_Ptr layer = checked_forest_layer(it->layer());
				if(!layer || !layer->has_node(it->index())) throw Exception(OSSWrapper() << "Invalid node: " << *it);
				if(it->layer() != commonLayer) throw Exception("Nodes not in same layer");
			}
		}

		CommandSequenceGuard guard(m_commandManager, "Merge Non-Sibling Nodes");

		std::set<PFNodeID> mergedNodes;

		// Calculate the connected components of the nodes to be merged.
		int layerIndex = nodes.begin()->layer();
		std::set<int> indices;
		for(std::set<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
		{
			indices.insert(it->index());
		}
		std::vector<std::set<int> > connectedComponents = find_connected_components(indices, layerIndex);

		// Arrange for the nodes in each connected component of size > 1 to be merged.
		for(std::vector<std::set<int> >::const_iterator it=connectedComponents.begin(), iend=connectedComponents.end(); it!=iend; ++it)
		{
			const std::set<int>& component = *it;
			if(component.size() == 1) continue;		// nothing to do

			// Find the layer to which the nodes need to be unzipped.
			int toLayer = find_common_ancestor_layer(component, layerIndex) - 1;	// i.e. the layer below the nodes' common ancestor

			// Unzip each node in the component to the specified layer, in each case keeping the chain that corresponds to the unzipped node itself,
			// which (by construction) will be the first one in the returned vector. Since we want the actual nodes (and not just the nodes above
			// them in their chains) to be merged, we add them to the ends of their respective chains here as well.
			std::vector<Chain> chains;
			chains.reserve(component.size());
			for(std::set<int>::const_iterator jt=component.begin(), jend=component.end(); jt!=jend; ++jt)
			{
				PFNodeID node(layerIndex, *jt);
				std::vector<Chain> unzipResult = unzip_node(node, toLayer, DONT_CHECK_PRECONDITIONS);
				chains.push_back(unzipResult.front());
				chains.back().push_back(node);
			}

			// Zip the chains together to effectuate the merge and store the resulting node.
			mergedNodes.insert(zip_chains(chains, DONT_CHECK_PRECONDITIONS).first);
		}

		return mergedNodes;
	}

	/**
	@brief	Switches a node from being the child of one parent in the layer above to the child of another.

	It is possible that moving the node may cause some of its old ancestors to become disconnected. If this happens,
	what remains of each old ancestor is split into its connected components. (This all happens automatically, as the
	operation is implemented in terms of zipping operations.)

	@note	This method executes a command sequence (which can be atomically undone, if an UndoableCommandManager
			has been previously installed using set_command_manager()). Listeners will be alerted to the individual
			changes involved, rather than the execution of the sequence as a whole.

	@param[in]	node				The node whose parent is to be switched
	@param[in]	newParent			The index of the new parent to which to switch it (in the layer above)
	@param[in]	checkPreconditions	Whether or not the preconditions need to be explicitly checked (default: yes)
	@pre
		-	The node itself is valid
		-	The proposed new parent is valid
		-	The node is adjacent to at least one child of its proposed new parent
	@post
		-	The partition forest will have been consistently modified so as to switch the parent of the node
	@throw Exception
		-	If the preconditions are violated
	*/
	void parent_switch(const PFNodeID& node, int newParent, CheckPreconditions checkPreconditions = CHECK_PRECONDITIONS)
	{
		if(checkPreconditions)
		{
			// Check that the node itself is a valid node.
			IForestLayer_Ptr layerN = checked_forest_layer(node.layer());
			if(!layerN || !layerN->has_node(node.index())) throw Exception(OSSWrapper() << "Invalid node: " << node);

			// Check that the proposed new parent is a valid node.
			BranchLayer_Ptr layerP = checked_branch_layer(node.layer() + 1);
			if(!layerP || !layerP->has_node(newParent)) throw Exception(OSSWrapper() << "Invalid node: " << PFNodeID(node.layer() + 1, newParent));

			// Check that the node is adjacent to at least one child of its proposed new parent.
			bool adjacent = false;
			const std::set<int>& children = layerP->node_children(newParent);
			for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
			{
				if(layerN->has_edge(node.index(), *it))
				{
					adjacent = true;
					break;
				}
			}
			if(!adjacent) throw Exception(OSSWrapper() << "Node " << node << " is not adjacent to its proposed new parent " << newParent);
		}

		CommandSequenceGuard guard(m_commandManager, "Parent Switch");

		// Find the common ancestor layer of the old and new parents, and the chain leading down to the new parent.
		IForestLayer_Ptr layerN = forest_layer(node.layer());
		int oldParent = layerN->node_parent(node.index());
		int commonAncestorLayer;
		Chain newChain;
		boost::tie(commonAncestorLayer, newChain) = find_common_ancestor_layer_and_new_chain(oldParent, newParent, node.layer() + 1);

		// Unzip the node to the layer below the common ancestor layer.
		std::vector<Chain> unzipResult = unzip_node(node, commonAncestorLayer - 1, DONT_CHECK_PRECONDITIONS);

		// Zip the chain leading upwards from the node being moved to the new chain to complete the parent switch.
		// Note that the old chain required is the first chain in the unzip result by construction -- see unzip_node().
		const Chain& oldChain = unzipResult.front();
		std::vector<Chain> chains;
		chains.push_back(oldChain);
		chains.push_back(newChain);
		zip_chains(chains, DONT_CHECK_PRECONDITIONS);
	}

	//@}

	//#################### PRIVATE METHODS ####################
private:
	bool are_connected(std::set<int> nodes, int layerIndex) const
	{
		if(nodes.empty()) return false;

		find_connected_component(nodes, layerIndex);

		// If no nodes remain after finding the first connected component, the nodes must be connected, and vice-versa.
		return nodes.empty();
	}

	BranchLayer_Ptr branch_layer(int index) const
	{
		return m_branchLayers[index-1];
	}

	BranchLayer_Ptr checked_branch_layer(int index) const
	{
		if(index >= 1 && index <= highest_layer()) return branch_layer(index);
		else return BranchLayer_Ptr();
	}

	IForestLayer_Ptr checked_forest_layer(int index) const
	{
		if(index >= 0 && index <= highest_layer()) return forest_layer(index);
		else return IForestLayer_Ptr();
	}

	void clone_layer_impl(int indexB)
	{
		// Note: We denote the layer below the clone as B and the clone layer itself as C.

		// Clone the graph of the layer below to make a new layer.
		IForestLayer_Ptr layerB = forest_layer(indexB);
		BranchLayer_Ptr layerC = clone_graph(*layerB);
		m_branchLayers.insert(m_branchLayers.begin() + indexB, layerC);

		// Copy each parent link from layer B to layer C, then update the link in B and create a child link in C.
		typename IForestLayerT::NodeIterator bt = layerB->nodes_begin(), bend = layerB->nodes_end();
		typename BranchLayer::BranchNodeIterator ct = layerC->branch_nodes_begin(), cend = layerC->branch_nodes_end();
		while(bt != bend)
		{
			ct->set_parent(bt->parent());
			bt->set_parent(bt.index());
			ct->children().insert(bt.index());
			++bt, ++ct;
		}

		m_listeners.layer_was_cloned(indexB);
	}

	static BranchLayer_Ptr clone_graph(const IForestLayer<BranchProperties,int>& sourceLayer)
	{
		BranchLayer_Ptr ret(new BranchLayer);

		std::vector<int> nodes = sourceLayer.node_indices();
		for(size_t i=0, size=nodes.size(); i<size; ++i)
		{
			std::set<int> children;
			children.insert(nodes[i]);
			ret->set_node_properties(nodes[i], sourceLayer.combine_properties(children));
		}

		for(typename IForestLayer<BranchProperties,int>::EdgeConstIterator it=sourceLayer.edges_cbegin(), iend=sourceLayer.edges_cend(); it!=iend; ++it)
		{
			ret->set_edge_weight(it->u, it->v, it->weight);
		}

		return ret;
	}

	BranchLayer_Ptr delete_layer_impl(int indexD)
	{
		m_listeners.layer_will_be_deleted(indexD);

		// Note: We denote the layer to be deleted as D, the layer below as B, and the layer above (if any) as A.
		// We know that the layer we're deleting is a branch layer, since deleting the leaf layer is explicitly prohibited.
		BranchLayer_Ptr layerD = branch_layer(indexD);

		// The parent of each node in layer B should be set to the parent of its parent in layer D. Conversely, the child set
		// of each node in layer A (if any) should be set to the union of the child sets of its children in layer D. This is
		// accomplished in two stages: firstly, the child sets of the nodes in layer A are cleared; secondly, we iterate over
		// the nodes in layer B and update their parent links to point to their grandparents in layer A, adding corresponding
		// child links from the grandparents in layer A as we do so.

		BranchLayer_Ptr layerA = checked_branch_layer(indexD + 1);
		if(layerA)
		{
			for(typename BranchLayer::BranchNodeIterator it=layerA->branch_nodes_begin(), iend=layerA->branch_nodes_end(); it!=iend; ++it)
			{
				it->children().clear();
			}
		}

		IForestLayer_Ptr layerB = forest_layer(indexD - 1);
		for(typename IForestLayerT::NodeIterator it=layerB->nodes_begin(), iend=layerB->nodes_end(); it!=iend; ++it)
		{
			int grandparentA = layerD->node_parent(it->parent());
			it->set_parent(grandparentA);
			if(layerA) layerA->node_children(grandparentA).insert(it.index());
		}

		// Now layer D itself can be deleted.
		m_branchLayers.erase(m_branchLayers.begin() + indexD - 1);

		m_listeners.layer_was_deleted(indexD);
		return layerD;
	}

	int find_common_ancestor_layer(const std::set<int>& component, int layerIndex) const
	{
		std::set<int> curs = component;
		while(layerIndex <= highest_layer() && curs.size() > 1)
		{
			IForestLayer_Ptr layer = forest_layer(layerIndex);
			std::set<int> parents;
			for(std::set<int>::const_iterator it=curs.begin(), iend=curs.end(); it!=iend; ++it)
			{
				parents.insert(layer->node_parent(*it));
			}
			curs = parents;
			++layerIndex;
		}
		return layerIndex;
	}

	std::pair<int,Chain> find_common_ancestor_layer_and_new_chain(int oldParent, int newParent, int layerIndex) const
	{
		Chain newChain;

		int curOld = oldParent, curNew = newParent;
		while(layerIndex <= highest_layer() && curOld != curNew)
		{
			newChain.push_front(PFNodeID(layerIndex, curNew));

			IForestLayer_Ptr layer = forest_layer(layerIndex);
			curOld = layer->node_parent(curOld);
			curNew = layer->node_parent(curNew);
			++layerIndex;
		}

		return std::make_pair(layerIndex, newChain);
	}

	std::set<int> find_connected_component(std::set<int>& nodes, int layerIndex) const
	{
		assert(!nodes.empty());

		std::set<int> ret;

		IForestLayer_Ptr layer = forest_layer(layerIndex);
		int seed = *nodes.begin();
		nodes.erase(nodes.begin());

		std::queue<int> q;
		q.push(seed);
		while(!q.empty())
		{
			int n = q.front();
			q.pop();
			ret.insert(n);

			std::vector<Edge> adjEdges = layer->adjacent_edges(n);
			for(typename std::vector<Edge>::const_iterator it=adjEdges.begin(), iend=adjEdges.end(); it!=iend; ++it)
			{
				int other = (n == it->u) ? it->v : it->u;
				std::set<int>::iterator jt = nodes.find(other);
				if(jt != nodes.end())
				{
					q.push(other);
					nodes.erase(jt);
				}
			}
		}

		return ret;
	}

	std::vector<std::set<int> > find_connected_components(std::set<int> nodes, int layerIndex) const
	{
		std::vector<std::set<int> > ret;
		while(!nodes.empty())
		{
			ret.push_back(find_connected_component(nodes, layerIndex));
		}
		return ret;
	}

	IForestLayer_Ptr forest_layer(int index) const
	{
		if(index == 0) return m_leafLayer;
		else return m_branchLayers[index-1];
	}

	PFNodeID merge_sibling_nodes_impl(const std::set<PFNodeID>& nodes)
	{
		m_listeners.nodes_will_be_merged(nodes);

		PFNodeID canonical = *nodes.begin();
		std::set<PFNodeID>::const_iterator othersBegin = nodes.begin(), othersEnd = nodes.end();
		++othersBegin;

		BranchLayer_Ptr layerM = branch_layer(canonical.layer());		// the layer in which the nodes are being merged
		IForestLayer_Ptr layerB = forest_layer(canonical.layer() - 1);	// the layer below that

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 1: Reconfigure the forest links
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		// Set the parent links of the children of the other nodes to point to the canonical node,
		// and conversely add the children of the other nodes as children of the canonical node.
		std::set<int>& canonicalChildren = layerM->node_children(canonical.index());
		for(std::set<PFNodeID>::const_iterator it=othersBegin, iend=othersEnd; it!=iend; ++it)
		{
			std::set<int>& children = layerM->node_children(it->index());
			for(std::set<int>::const_iterator jt=children.begin(), jend=children.end(); jt!=jend; ++jt)
			{
				layerB->set_node_parent(*jt, canonical.index());
				canonicalChildren.insert(*jt);
			}
#if 0
			children.clear();	// redundant, but makes what's going on easier to understand when debugging
#endif
		}

		// If we are not in the highest layer of the hierarchy, remove the child links between the common parent and the other nodes.
		if(canonical.layer() != highest_layer())
		{
			BranchLayer_Ptr layerA = branch_layer(canonical.layer() + 1);
			int parentIndex = layerM->node_parent(canonical.index());
			std::set<int>& parentChildren = layerA->node_children(parentIndex);
			for(std::set<PFNodeID>::const_iterator it=othersBegin, iend=othersEnd; it!=iend; ++it)
			{
				parentChildren.erase(it->index());
#if 0
				layerM->set_node_parent(it->index(), -1);	// redundant, but makes what's going on easier to understand when debugging
#endif
			}
		}

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 2: Recalculate the adjacent edges and properties for the canonical node
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		// Update the adjacent edges of the canonical node based on those of the other nodes.
		for(std::set<PFNodeID>::const_iterator it=othersBegin, iend=othersEnd; it!=iend; ++it)
		{
			std::vector<Edge> adjEdges = layerM->adjacent_edges(it->index());
			for(typename std::vector<Edge>::const_iterator jt=adjEdges.begin(), jend=adjEdges.end(); jt!=jend; ++jt)
			{
				int otherEnd = (it->index() == jt->u) ? jt->v : jt->u;
				if(otherEnd != canonical.index()) layerM->update_edge_weight(canonical.index(), otherEnd, jt->weight);
#if 0
				layerM->remove_edge(jt->u, jt->v);	// redundant, but makes what's going on easier to understand when debugging
#endif
			}
		}

		// Recalculate the properties for the canonical node.
		layerM->set_node_properties(canonical.index(), layerB->combine_properties(layerM->node_children(canonical.index())));

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 3: Remove the other merged nodes (note that this also removes their adjacent edges)
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		for(std::set<PFNodeID>::const_iterator it=othersBegin, iend=othersEnd; it!=iend; ++it)
		{
			layerM->remove_node(it->index());
		}

		m_listeners.nodes_were_merged(nodes, canonical);
		return canonical;
	}

	std::set<PFNodeID> split_node_impl(const PFNodeID& node, const std::vector<std::set<int> >& groups)
	{
		BranchLayer_Ptr layerA = checked_branch_layer(node.layer() + 1);
		BranchLayer_Ptr layerS = branch_layer(node.layer());
		IForestLayer_Ptr layerB = forest_layer(node.layer() - 1);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 1: Delete the node being split from the forest
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		// Remove any forest links which reference the node.
		std::set<int> children = layerS->node_children(node.index());
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			layerB->set_node_parent(*it, -1);
		}

		int parentIndex = layerS->node_parent(node.index());
		if(layerA) layerA->node_children(parentIndex).erase(node.index());

		// Remove the node from its partitioning graph.
		layerS->remove_node(node.index());

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 2: Add new nodes for each of the groups to the split layer, along with the appropriate forest links.
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		std::set<PFNodeID> newNodes;
		for(std::vector<std::set<int> >::const_iterator it=groups.begin(), iend=groups.end(); it!=iend; ++it)
		{
			const std::set<int>& group = *it;
			int groupIndex = *it->begin();		// note that the groups are guaranteed to be non-empty by the method preconditions (checked as necessary)
			newNodes.insert(PFNodeID(node.layer(), groupIndex));
			layerS->set_node_properties(groupIndex, layerB->combine_properties(group));
			layerS->set_node_children(groupIndex, group);
			layerS->set_node_parent(groupIndex, parentIndex);
			for(std::set<int>::const_iterator jt=group.begin(), jend=group.end(); jt!=jend; ++jt) layerB->set_node_parent(*jt, groupIndex);
			if(layerA) layerA->node_children(parentIndex).insert(groupIndex);
		}

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 3: Propagate the necessary edges from the child layer to the split layer.
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		for(std::vector<std::set<int> >::const_iterator it=groups.begin(), iend=groups.end(); it!=iend; ++it)
		{
			for(std::set<int>::const_iterator jt=it->begin(), jend=it->end(); jt!=jend; ++jt)
			{
				std::vector<Edge> adjEdges = layerB->adjacent_edges(*jt);
				for(typename std::vector<Edge>::const_iterator kt=adjEdges.begin(), kend=adjEdges.end(); kt!=kend; ++kt)
				{
					int parentU = layerB->node_parent(kt->u), parentV = layerB->node_parent(kt->v);
					if(parentU != parentV) layerS->update_edge_weight(parentU, parentV, kt->weight);
				}
			}
		}

		m_listeners.node_was_split(node, newNodes);
		return newNodes;
	}

	void undelete_layer_impl(int indexD, const BranchLayer_Ptr& layerD)
	{
		// Note: We denote the layer which has been deleted as D, the layer below as B, and the layer above (if any) as A.

		// Insert the deleted layer back into the hierarchy.
		// Note: The brackets around indexD - 1 are NOT optional. Without them, it's possible to overrun the end of the container when adding indexD.
		m_branchLayers.insert(m_branchLayers.begin() + (indexD - 1), layerD);

		// Recreate the parent links in layer B and the child links in layer A (if it exists).
		BranchLayer_Ptr layerA = checked_branch_layer(indexD + 1);
		if(layerA)
		{
			for(typename BranchLayer::BranchNodeIterator it=layerA->branch_nodes_begin(), iend=layerA->branch_nodes_end(); it!=iend; ++it)
			{
				it->children().clear();
			}
		}

		IForestLayer_Ptr layerB = forest_layer(indexD - 1);
		for(typename BranchLayer::BranchNodeIterator it=layerD->branch_nodes_begin(), iend=layerD->branch_nodes_end(); it!=iend; ++it)
		{
			const std::set<int>& children = it->children();
			for(std::set<int>::const_iterator jt=children.begin(), jend=children.end(); jt!=jend; ++jt)
			{
				layerB->set_node_parent(*jt, it.index());
			}
			if(layerA) layerA->node_children(it->parent()).insert(it.index());
		}

		// Alert any forest listeners as necessary.
		m_listeners.layer_was_undeleted(indexD);
	}
};

}

using mp_PartitionForest::PartitionForest;

}

#endif
