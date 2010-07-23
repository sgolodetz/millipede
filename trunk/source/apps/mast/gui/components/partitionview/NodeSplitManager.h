/***
 * millipede: NodeSplitManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_NODESPLITMANAGER
#define H_MILLIPEDE_NODESPLITMANAGER

#include <common/listeners/CompositeListenerBase.h>
#include <common/partitionforests/base/PartitionForest.h>

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

	//#################### PRIVATE VARIABLES ####################
private:
	PartitionForest_Ptr m_forest;
	CompositeListener m_listeners;
	PFNodeID m_splitNode;
	std::vector<std::set<PFNodeID> > m_subgroups;
	std::set<PFNodeID> m_unallocatedChildren;

	//#################### CONSTRUCTORS ####################
public:
	explicit NodeSplitManager(const PartitionForest_Ptr& forest)
	:	m_forest(forest)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void add_shared_listener(const boost::shared_ptr<Listener>& listener)
	{
		m_listeners.add_shared_listener(listener);
	}

	void add_subgroup(const std::set<PFNodeID>& subgroup)
	{
		// TODO
	}

	void finalize_split()
	{
		// TODO
	}

	void forest_changed(int commandDepth)
	{
		reset();
	}

	void remove_subgroup_containing(const PFNodeID& node)
	{
		// TODO
	}

	void reset()
	{
		m_splitNode = PFNodeID::invalid();
		std::vector<std::set<PFNodeID> >().swap(m_subgroups);
		m_unallocatedChildren.clear();

		m_listeners.node_split_manager_changed();
	}

	void set_split_node(const PFNodeID& splitNode)
	{
		reset();
		m_splitNode = splitNode;
		m_unallocatedChildren = m_forest->children_of(splitNode);

		m_listeners.node_split_manager_changed();
	}

	const std::set<PFNodeID>& unallocated_children() const
	{
		return m_unallocatedChildren;
	}
};

}

#endif
