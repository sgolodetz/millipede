/***
 * millipede: PartitionForestTouchListener.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFORESTTOUCHLISTENER
#define H_MILLIPEDE_PARTITIONFORESTTOUCHLISTENER

#include "PartitionForest.h"

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class PartitionForestTouchListener : public PartitionForest<LeafLayer,BranchLayer>::Listener
{
	//#################### TYPEDEFS ####################
private:
	typedef std::set<int> Layer;

	//#################### PRIVATE VARIABLES ####################
private:
	bool m_dirty;
	std::vector<Layer> m_nodes;

	//#################### CONSTRUCTORS ####################
public:
	explicit PartitionForestTouchListener(int highestLayer)
	{
		reset(highestLayer + 1);
	}

	//#################### PRIVATE ABSTRACT METHODS ####################
public:
	virtual void nodes_were_touched(const std::vector<Layer>& nodes) = 0;

	//#################### PUBLIC METHODS ####################
public:
	void forest_changed(int commandDepth)
	{
		if(commandDepth == 0 && m_dirty)
		{
			nodes_were_touched(m_nodes);
			reset(m_nodes.size());
		}
	}

	void layer_was_cloned(int index)
	{
		reset(m_nodes.size() + 1);
		forest_changed(0);
	}

	void layer_was_deleted(int index)
	{
		reset(m_nodes.size() - 1);
		forest_changed(0);
	}

	void layer_was_undeleted(int index)
	{
		reset(m_nodes.size() + 1);
		forest_changed(0);
	}

	void node_was_split(const PFNodeID& node, const std::set<PFNodeID>& results, int commandDepth)
	{
		Layer& layer = m_nodes[node.layer()];
		layer.erase(node.index());
		for(std::set<PFNodeID>::const_iterator it=results.begin(), iend=results.end(); it!=iend; ++it)
		{
			layer.insert(it->index());
		}

		m_dirty = true;
		forest_changed(commandDepth);
	}

	void nodes_were_merged(const std::set<PFNodeID>& nodes, const PFNodeID& result, int commandDepth)
	{
		Layer& layer = m_nodes[result.layer()];
		for(std::set<PFNodeID>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
		{
			layer.erase(it->index());
		}
		layer.insert(result.index());

		m_dirty = true;
		forest_changed(commandDepth);
	}

	//#################### PRIVATE METHODS ####################
private:
	void reset(int layerCount)
	{
		std::vector<Layer>(layerCount).swap(m_nodes);
		m_dirty = false;
	}
};

}

#endif
