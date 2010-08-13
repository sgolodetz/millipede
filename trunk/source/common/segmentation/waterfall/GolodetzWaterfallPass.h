/***
 * millipede: GolodetzWaterfallPass.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_GOLODETZWATERFALLPASS
#define H_MILLIPEDE_GOLODETZWATERFALLPASS

#include <map>

#include "WaterfallPass.h"

namespace mp {

template <typename EdgeWeight>
class GolodetzWaterfallPass : public WaterfallPass<EdgeWeight>
{
	//#################### ENUMERATIONS ####################
private:
	enum NodeClassifier
	{
		AMBIGUOUS_IN,
		AMBIGUOUS_OUT,
		NO_FLOW,
		UNAMBIGUOUS_IN,
		UNAMBIGUOUS_OUT,
	};

	//#################### NESTED CLASSES ####################
private:
	struct NodeData
	{
		std::set<int> m_arrows;		// the nodes at the other ends of arrowed edges (ones along which water would flow)
		bool m_checkParent;			// whether the parent route needs checking in the down pass
		int m_distance;				// the node's distance value (see algorithm description)
		bool m_parentWillMerge;		// whether the parent edge of this node will be merged

		NodeData()
		:	m_checkParent(false), m_distance(0), m_parentWillMerge(false)
		{}
	};

	//#################### PUBLIC METHODS ####################
public:
	RootedMST<EdgeWeight>& run(RootedMST<EdgeWeight>& mst)
	{
		if(mst.node_count() > 1)
		{
			up_pass(mst, mst.tree_root());
			down_pass(mst, mst.tree_root());
			merge_pass(mst, mst.tree_root());
		}
		return mst;
	}

	//#################### PRIVATE METHODS ####################
private:
	static NodeClassifier classify_node(const NodeData& data, int other)
	{
		NodeClassifier classifier;
		switch(data.m_arrows.size())
		{
			case 0:
			{
				classifier = NO_FLOW;
				break;
			}
			case 1:
			{
				classifier = *data.m_arrows.begin() == other ? UNAMBIGUOUS_IN : UNAMBIGUOUS_OUT;
				break;
			}
			default:
			{
				classifier = data.m_arrows.find(other) != data.m_arrows.end() ? AMBIGUOUS_IN : AMBIGUOUS_OUT;
				break;
			}
		}
		return classifier;
	}

	void down_pass(RootedMST<EdgeWeight>& mst, int cur)
	{
		int parent = mst.tree_parent(cur);

		// Check for a better upwards route if necessary.
		NodeData data = mst.node_data<NodeData>(cur);
		if(data.m_checkParent)
		{
			const NodeData& parentData = mst.node_data<NodeData>(parent);
			if(parentData.m_arrows.find(cur) == parentData.m_arrows.end())
			{
				// There is a potential upwards route.
				int upwardsDistance = parentData.m_distance + 1;
				if(upwardsDistance < data.m_distance)
				{
					// The upwards route is strictly better.
					data.m_arrows.clear();
					data.m_arrows.insert(parent);
					data.m_distance = upwardsDistance;
				}
				else if(upwardsDistance == data.m_distance)
				{
					// The upwards route is just as good.
					data.m_arrows.insert(parent);
				}
			}
		}

		// Determine whether the parent edge (if any) should be merged.
		if(parent != -1)
		{
			const NodeData& parentData = mst.node_data<NodeData>(parent);
			NodeClassifier curClassifier = classify_node(data, parent);
			NodeClassifier parentClassifier = classify_node(parentData, cur);
			data.m_parentWillMerge = curClassifier == UNAMBIGUOUS_IN || parentClassifier == UNAMBIGUOUS_IN || (curClassifier == NO_FLOW && parentClassifier == NO_FLOW);
		}

		mst.set_node_data(cur, data);

		// Recurse on the children.
		std::set<int> children = mst.tree_children(cur);
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			down_pass(mst, *it);
		}
	}

	int merge_pass(RootedMST<EdgeWeight>& mst, int cur)
	{
		// Recurse on the children.
		std::set<int> children = mst.tree_children(cur);
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			cur = merge_pass(mst, *it);
		}

		// Merge the parent edge if necessary.
		int parent = mst.tree_parent(cur);
		const NodeData& data = mst.node_data<NodeData>(cur);
		if(data.m_parentWillMerge)
		{
			NodeData parentData = mst.node_data<NodeData>(parent);
			int oldParent = parent;
			parent = this->merge_nodes(mst, parent, cur);

			// Ensure that the data associated with the new parent node is correct.
			parentData.m_arrows.erase(cur);
			parentData.m_arrows.insert(-cur);	// the - is to represent a node which no longer exists
			mst.set_node_data(parent, parentData);

			if(parent != oldParent)
			{
				// Ensure that the new parent's parent (if any) and children refer to it by its new name.
				int grandparent = mst.tree_parent(parent);
				if(grandparent != -1)
				{
					NodeData grandparentData = mst.node_data<NodeData>(grandparent);
					if(grandparentData.m_arrows.find(oldParent) != grandparentData.m_arrows.end())
					{
						grandparentData.m_arrows.erase(oldParent);
						grandparentData.m_arrows.insert(parent);
						mst.set_node_data(grandparent, grandparentData);
					}
				}

				std::set<int> children = mst.tree_children(parent);
				for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
				{
					NodeData childData = mst.node_data<NodeData>(*it);
					if(childData.m_arrows.find(oldParent) != childData.m_arrows.end())
					{
						childData.m_arrows.erase(oldParent);
						childData.m_arrows.insert(parent);
						mst.set_node_data(*it, childData);
					}
				}
			}
		}

		return parent;
	}

	void up_pass(RootedMST<EdgeWeight>& mst, int cur)
	{
		int parent = mst.tree_parent(cur);
		NodeData data;

		// Recursively process any children.
		std::set<int> children = mst.tree_children(cur);
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			up_pass(mst, *it);
		}

		// Construct the weight -> edges map.
		std::map<EdgeWeight,std::set<int> > weightToEdges;
		if(parent != -1)
		{
			weightToEdges[mst.edge_weight(parent, cur)].insert(parent);
		}
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			weightToEdges[mst.edge_weight(cur, *it)].insert(*it);
		}

		// If this node has a unique edge of steepest descent (i.e. a unique lowest-valued edge
		// leading out of it, which can be the parent edge), add an arrow on the node pointing
		// along the edge and early-out.
		const std::set<int>& lowestEdges = weightToEdges.begin()->second;
		if(lowestEdges.size() == 1)
		{
			data.m_arrows = lowestEdges;
			mst.set_node_data(cur, data);
			return;
		}

		// Otherwise, consider all the lowest-valued child edges (i.e. ignore the parent for now,
		// even if it is also a lowest-valued edge) which satisfy the two conditions:
		//
		// (a)	There is no arrow on the node at the other end of the edge pointing along the
		//		edge towards this node
		// (b)	The node at the other end of the edge has a distance value not equal to INT_MAX
		//
		// If there are no such child edges, then this node is unescapable along a child edge and
		// should be given a distance value of INT_MAX. Otherwise, each of these child edges can be
		// assigned a distance value equal to 1 greater than the value on the node at the other end of
		// the edge: this value on the node will be 0 for nodes from which there is a unique path of
		// steepest descent, and non-zero otherwise. Pick all the child edges whose distance value is
		// minimal and add arrows to this node pointing along them (these indicate the initial paths
		// of steepest descent from this node). Store the minimum distance value as this node's value.

		std::set<int> lowestChildEdges = lowestEdges;
		lowestChildEdges.erase(parent);
		for(std::set<int>::iterator it=lowestChildEdges.begin(), iend=lowestChildEdges.end(); it!=iend; /* No-op */)
		{
			const NodeData& childData = mst.node_data<NodeData>(*it);
			if(childData.m_arrows.find(cur) != childData.m_arrows.end() || childData.m_distance == INT_MAX)
			{
				lowestChildEdges.erase(it++);
			}
			else ++it;
		}

		if(!lowestChildEdges.empty())
		{
			std::map<int,std::set<int> > distanceToEdges;
			for(std::set<int>::const_iterator it=lowestChildEdges.begin(), iend=lowestChildEdges.end(); it!=iend; ++it)
			{
				const NodeData& childData = mst.node_data<NodeData>(*it);
				distanceToEdges[childData.m_distance+1].insert(*it);
			}

			int minDistance = distanceToEdges.begin()->first;
			const std::set<int>& minEdges = distanceToEdges.begin()->second;

			data.m_arrows = minEdges;
			data.m_distance = minDistance;
		}
		else
		{
			data.m_distance = INT_MAX;
		}

		// Now consider the parent edge: if it was a lowest-valued edge, mark it as a potential
		// path of steepest descent to avoid the need to check again in the down pass.
		if(lowestEdges.find(parent) != lowestEdges.end())
		{
			data.m_checkParent = true;
		}

		mst.set_node_data(cur, data);
	}
};

}

#endif
