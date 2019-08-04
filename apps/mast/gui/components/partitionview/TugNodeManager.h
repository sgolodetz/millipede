/***
 * millipede: TugNodeManager.h
 * Copyright Stuart Golodetz, 2017. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TUGNODEMANAGER
#define H_MILLIPEDE_TUGNODEMANAGER

#include <millipede/listeners/CompositeListenerBase.h>
#include <millipede/partitionforests/base/PartitionForestSelection.h>

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class TugNodeManager : public PartitionForest<LeafLayer,BranchLayer>::Listener
{
	//#################### LISTENERS ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void tug_node_manager_changed() = 0;
	};

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void tug_node_manager_changed()
		{
			multicast(boost::bind(&Listener::tug_node_manager_changed, _1));
		}
	};

	//#################### TYPEDEFS ####################
private:
	typedef PartitionForest<LeafLayer,BranchLayer> PartitionForestT;
	typedef boost::shared_ptr<PartitionForestT> PartitionForest_Ptr;
	typedef PartitionForestSelection<LeafLayer,BranchLayer> PartitionForestSelectionT;
	typedef boost::shared_ptr<PartitionForestSelectionT> PartitionForestSelection_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	std::set<PFNodeID> m_adjacentNodes;
	int m_atomicLayer;
	PartitionForest_Ptr m_forest;
	CompositeListener m_listeners;
	PFNodeID m_node;
	PartitionForestSelection_Ptr m_selection;
	int m_toLayerOffset;
	TugMode m_tugMode;

	//#################### CONSTRUCTORS ####################
public:
	TugNodeManager(const PartitionForest_Ptr& forest, const PartitionForestSelection_Ptr& selection)
	:	m_forest(forest), m_selection(selection)
	{
		reset();
	}

	//#################### PUBLIC METHODS ####################
public:
	bool active() const
	{
		return m_node != PFNodeID::invalid();
	}

	const std::set<PFNodeID>& adjacent_nodes() const
	{
		return m_adjacentNodes;
	}

	void add_shared_listener(const boost::shared_ptr<Listener>& listener)
	{
		m_listeners.add_shared_listener(listener);
	}

	int atomic_layer() const
	{
		return m_atomicLayer;
	}

	void forest_changed(int commandDepth)
	{
		reset();
	}

	const PFNodeID& node() const
	{
		return m_node;
	}

	void perform_tug()
	{
		if(active())
		{
			PartitionForestCut_CPtr cut(new HorizontalCut(m_node.layer() + m_toLayerOffset));
			m_forest->tug_node(m_node, m_atomicLayer, cut, m_tugMode);
			reset();
		}
	}

	void reset()
	{
		m_adjacentNodes.clear();
		m_atomicLayer = 1;
		m_node = PFNodeID::invalid();
		m_toLayerOffset = 0;
		m_tugMode = TUGMODE_SIMPLE;

		m_listeners.tug_node_manager_changed();
	}

	void set_atomic_layer(int atomicLayer)
	{
		m_atomicLayer = atomicLayer;
		recalculate_adjacent_nodes();

		m_listeners.tug_node_manager_changed();
	}

	void set_node(const PFNodeID& node)
	{
		m_node = node;
		recalculate_adjacent_nodes();

		m_listeners.tug_node_manager_changed();
	}

	void set_to_layer_offset(int toLayerOffset)
	{
		m_toLayerOffset = toLayerOffset;

		m_listeners.tug_node_manager_changed();
	}

	void set_tug_mode(TugMode tugMode)
	{
		m_tugMode = tugMode;
	}

	int to_layer() const
	{
		return m_node != PFNodeID::invalid() ? m_node.layer() + m_toLayerOffset : -1;
	}

	int to_layer_offset() const
	{
		return m_toLayerOffset;
	}

	//#################### PRIVATE METHODS ####################
private:
	void recalculate_adjacent_nodes()
	{
		// If the target node is currently invalid, early out.
		if(m_node == PFNodeID::invalid()) return;

		// Get the target node's descendants in the atomic layer.
		// FIXME: It would be better to use the forest directly for this rather than relying on the selection.
		std::set<PFNodeID> nodes(m_selection->view_at_layer_cbegin(m_atomicLayer), m_selection->view_at_layer_cend(m_atomicLayer));

		// Calculate the nodes that are adjacent to the target node's descendants in the atomic layer.
		m_adjacentNodes.clear();
		for(std::set<PFNodeID>::const_iterator it = nodes.begin(), iend = nodes.end(); it != iend; ++it)
		{
			std::vector<int> temp = m_forest->adjacent_nodes(*it);
			for(size_t j = 0, size = temp.size(); j < size; ++j)
			{
				PFNodeID adjacentNode(m_atomicLayer, temp[j]);
				if(nodes.find(adjacentNode) == nodes.end())
				{
					m_adjacentNodes.insert(adjacentNode);
				}
			}
		}
	}
};

}

#endif
