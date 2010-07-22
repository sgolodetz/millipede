/***
 * millipede: ParentSwitchManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARENTSWITCHMANAGER
#define H_MILLIPEDE_PARENTSWITCHMANAGER

#include <common/exceptions/Exception.h>
#include <common/partitionforests/base/PartitionForest.h>

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class ParentSwitchManager : public PartitionForest<LeafLayer,BranchLayer>::Listener
{
	//#################### TYPEDEFS ####################
private:
	typedef PartitionForest<LeafLayer,BranchLayer> PartitionForestT;
	typedef boost::shared_ptr<PartitionForestT> PartitionForest_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	PartitionForest_Ptr m_forest;
	PFNodeID m_child;
	std::set<PFNodeID> m_potentialNewParents;

	//#################### CONSTRUCTORS ####################
public:
	explicit ParentSwitchManager(const PartitionForest_Ptr& forest)
	:	m_forest(forest)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void forest_changed(int commandDepth)
	{
		reset();
	}

	bool has_child() const
	{
		return m_child != PFNodeID::invalid();
	}

	void perform_switch(const PFNodeID& newParent)
	{
		if(m_potentialNewParents.find(newParent) != m_potentialNewParents.end())
		{
			m_forest->parent_switch(m_child, newParent.index());
			reset();
		}
		else throw Exception("The specified new parent is invalid");
	}

	const std::set<PFNodeID>& potential_new_parents() const
	{
		return m_potentialNewParents;
	}

	void reset()
	{
		m_child = PFNodeID::invalid();
		m_potentialNewParents.clear();
	}

	void set_child(const PFNodeID& child)
	{
		reset();
		if(child.layer() == m_forest->highest_layer()) return;
		m_child = child;

		// Calculate the potential new parents.
		std::vector<int> adjChildren = m_forest->adjacent_nodes(child);
		for(size_t i=0, size=adjChildren.size(); i<size; ++i)
		{
			// Note: We know that the parent nodes are valid because we checked that the child wasn't in the highest forest layer above.
			m_potentialNewParents.insert(m_forest->parent_of(PFNodeID(child.layer(), adjChildren[i])));
		}

		// Note: The child's own parent is not a potential *new* parent (obviously).
		m_potentialNewParents.erase(m_forest->parent_of(child));
	}
};

}

#endif
