/***
 * millipede: NodeSplitManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_NODESPLITMANAGER
#define H_MILLIPEDE_NODESPLITMANAGER

#include <list>

#include <millipede/listeners/CompositeListenerBase.h>
#include <millipede/partitionforests/base/PartitionForestSelection.h>

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class NodeSplitManager : public PartitionForest<LeafLayer,BranchLayer>::Listener
{
	//#################### LISTENERS ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void node_split_manager_changed() = 0;
	};

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void node_split_manager_changed()
		{
			multicast(boost::bind(&Listener::node_split_manager_changed, _1));
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
	std::set<PFNodeID> m_allocatedChildren;
	PartitionForest_Ptr m_forest;
	CompositeListener m_listeners;
	PartitionForestSelection_Ptr m_selection;
	PFNodeID m_splitNode;
	std::list<std::set<PFNodeID> > m_subgroups;
	std::set<PFNodeID> m_unallocatedChildren;

	//#################### CONSTRUCTORS ####################
public:
	NodeSplitManager(const PartitionForest_Ptr& forest, const PartitionForestSelection_Ptr& selection)
	:	m_forest(forest), m_selection(selection)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void add_shared_listener(const boost::shared_ptr<Listener>& listener)
	{
		m_listeners.add_shared_listener(listener);
	}

	void add_subgroup(const std::set<PFNodeID>& subgroup)
	{
		// Note: No validation of the subgroup is done here - it's done in the update handler for the menu item instead.
		for(std::set<PFNodeID>::const_iterator it=subgroup.begin(), iend=subgroup.end(); it!=iend; ++it)
		{
			m_unallocatedChildren.erase(*it);
			m_allocatedChildren.insert(*it);
		}
		m_subgroups.push_back(subgroup);

		m_listeners.node_split_manager_changed();
	}

	const std::set<PFNodeID>& allocated_children() const
	{
		return m_allocatedChildren;
	}

	void finalize_split()
	{
		std::vector<std::set<int> > subgroups;

		// Add the known subgroups.
		for(std::list<std::set<PFNodeID> >::const_iterator it=m_subgroups.begin(), iend=m_subgroups.end(); it!=iend; ++it)
		{
			std::set<int> subgroup;
			for(std::set<PFNodeID>::const_iterator jt=it->begin(), jend=it->end(); jt!=jend; ++jt)
			{
				subgroup.insert(jt->index());
			}
			subgroups.push_back(subgroup);
		}

		// Determine the connected components of any unallocated children and add those as additional subgroups.
		std::set<int> unallocatedChildren;
		for(std::set<PFNodeID>::const_iterator it=m_unallocatedChildren.begin(), iend=m_unallocatedChildren.end(); it!=iend; ++it)
		{
			unallocatedChildren.insert(it->index());
		}
		std::vector<std::set<int> > connectedComponents = m_forest->find_connected_components(unallocatedChildren, m_splitNode.layer() - 1);
		std::copy(connectedComponents.begin(), connectedComponents.end(), std::back_inserter(subgroups));

		// Split the node.
		m_forest->split_node(m_splitNode, subgroups);

		reset();
	}

	void forest_changed(int commandDepth)
	{
		reset();
	}

	void remove_subgroup_containing(const PFNodeID& node)
	{
		for(std::list<std::set<PFNodeID> >::iterator it=m_subgroups.begin(), iend=m_subgroups.end(); it!=iend; ++it)
		{
			const std::set<PFNodeID>& subgroup = *it;
			if(subgroup.find(node) != subgroup.end())
			{
				for(std::set<PFNodeID>::const_iterator jt=subgroup.begin(), jend=subgroup.end(); jt!=jend; ++jt)
				{
					m_allocatedChildren.erase(*jt);
					m_unallocatedChildren.insert(*jt);
				}
				m_subgroups.erase(it);
				break;
			}
		}

		m_listeners.node_split_manager_changed();
	}

	void reset()
	{
		m_allocatedChildren.clear();
		m_splitNode = PFNodeID::invalid();
		m_subgroups.clear();
		m_unallocatedChildren.clear();

		m_listeners.node_split_manager_changed();
	}

	void set_split_node(const PFNodeID& splitNode)
	{
		reset();
		m_splitNode = splitNode;
		m_unallocatedChildren = m_forest->children_of(splitNode);

		// Clear the selection to make it easier for the user to see what's going on.
		m_selection->clear();

		m_listeners.node_split_manager_changed();
	}

	const PFNodeID& split_node() const
	{
		return m_splitNode;
	}

	const std::list<std::set<PFNodeID> >& subgroups() const
	{
		return m_subgroups;
	}

	const std::set<PFNodeID>& unallocated_children() const
	{
		return m_unallocatedChildren;
	}
};

}

#endif
