/***
 * millipede: PartitionForest.tpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <boost/tuple/tuple.hpp>

#include "OSSWrapper.h"

#define PartitionForest_HEADER	template <typename BranchProperties, typename LeafProperties, typename IDConverter>
#define PartitionForest_THIS	PartitionForest<BranchProperties,LeafProperties,IDConverter>

namespace mp {

//#################### NESTED CLASSES ####################
PartitionForest_HEADER
class PartitionForest_THIS::Node
{
	//#################### FRIENDS ####################
	friend class PartitionForest;

	//#################### PROTECTED VARIABLES ####################
protected:
	int m_parent;

	//#################### CONSTRUCTORS ####################
public:
	explicit Node(int parent)
	:	m_parent(parent)
	{}

	//#################### DESTRUCTOR ####################
public:
	virtual ~Node() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	int parent() const	{ return m_parent; }
};

PartitionForest_HEADER
class PartitionForest_THIS::Branch : public Node
{
	//#################### FRIENDS ####################
	friend class PartitionForest;

	//#################### PRIVATE VARIABLES ####################
private:
	std::set<int> m_children;
	BranchProperties m_properties;

	//#################### CONSTRUCTORS ####################
public:
	explicit Branch(const std::set<int>& children)
	:	Node(-1), m_children(children)
	{}

	//#################### PUBLIC METHODS ####################
public:
	const std::set<int>& children() const		{ return m_children; }
	const BranchProperties& properties() const	{ return m_properties; }
};

PartitionForest_HEADER
class PartitionForest_THIS::Leaf : public Node
{
	//#################### FRIENDS ####################
	friend class PartitionForest;

	//#################### PRIVATE VARIABLES ####################
private:
	LeafProperties m_properties;

	//#################### CONSTRUCTORS ####################
public:
	explicit Leaf(const LeafProperties& properties)
	:	Node(-1), m_properties(properties)
	{}

	//#################### PUBLIC METHODS ####################
public:
	const LeafProperties& properties() const	{ return m_properties; }
};

PartitionForest_HEADER
struct PartitionForest_THIS::NodeHandle
{
	int layer;
	int index;

	NodeHandle(int layer_, int index_)
	:	layer(layer_), index(index_)
	{}
};

//#################### CONSTRUCTORS ####################
PartitionForest_HEADER
PartitionForest_THIS::PartitionForest(const LeafLayer_Ptr& leafLayer, const BranchLayer_Ptr& lowestBranchLayer)
:	m_leafLayer(leafLayer)
{
	m_branchLayers.push_back(lowestBranchLayer);

	// Fill in the parent handles for the leaf layer and calculate the properties for the lowest branch layer.
	BranchLayer::NodeIterator_Ptr it = lowestBranchLayer->nodes();
	while(it->has_next())
	{
		int parentIndex;
		Branch_Ptr parent;
		boost::tie(parentIndex, parent) = it->next();

		const std::set<int>& children = parent->children();
		for(std::set<int>::const_iterator jt=children.begin(), jend=children.end(); jt!=jend; ++jt)
		{
			Leaf& child = (*leafLayer)(*jt);
			child.m_parent = parentIndex;
		}

		recalculate_properties(NodeHandle(1, parentIndex));
	}
}

//#################### PUBLIC METHODS ####################
PartitionForest_HEADER
void PartitionForest_THIS::clone_above_layer(int layer)
{
	check_branch_layer(layer);

	// Clone the layer.
	BranchLayer_Ptr originalLayer = m_branchLayers[layer-1];
	BranchLayer_Ptr clonedLayer(new BranchLayer(*originalLayer));

	std::vector<BranchLayer_Ptr>::iterator it = m_branchLayers.begin();
	std::advance(it, layer);

	m_branchLayers.insert(it, clonedLayer);

	// Fix up the parent and children handles in the original and cloned layers.
	BranchLayer::NodeIterator_Ptr jt = originalLayer->nodes();
	while(jt->has_next())
	{
		int index;
		Branch_Ptr child;
		boost::tie(index, child) = jt->next();

		child->m_parent = index;

		std::set<int> children;
		children.insert(index);
		(*clonedLayer)(index).m_children = children;
	}
}

PartitionForest_HEADER
void PartitionForest_THIS::clone_below_layer(int layer)
{
	// NYI
	throw 23;
}

PartitionForest_HEADER
const typename PartitionForest_THIS::Branch&
PartitionForest_THIS::get_branch(const NodeHandle& nh) const
{
	check_branch_layer(nh.layer);

	BranchLayer& layer = *m_branchLayers[nh.layer-1];
	if(layer.has_node(nh.index)) return layer(nh.index);
	else return layer(lookup_branch_index(nh));
}

PartitionForest_HEADER
const typename PartitionForest_THIS::BranchLayer&
PartitionForest_THIS::get_branch_layer(int layer) const
{
	check_branch_layer(layer);
	return *m_branchLayers[layer-1];
}

PartitionForest_HEADER
const typename PartitionForest_THIS::Leaf&
PartitionForest_THIS::get_leaf(int index) const
{
	return (*m_leafLayer)(index);
}

PartitionForest_HEADER
const typename PartitionForest_THIS::LeafLayer&
PartitionForest_THIS::get_leaf_layer() const
{
	return *m_leafLayer;
}

PartitionForest_HEADER
const typename PartitionForest_THIS::Node&
PartitionForest_THIS::get_node(const NodeHandle& nh) const
{
	if(nh.layer == 0) return get_leaf(nh.index);
	else return get_branch(nh);
}

PartitionForest_HEADER
int PartitionForest_THIS::highest_layer() const
{
	return static_cast<int>(m_branchLayers.size());
}

PartitionForest_HEADER
void PartitionForest_THIS::merge_siblings(int layer, const std::vector<int>& indices)
{
	// NYI
	throw 23;
}

PartitionForest_HEADER
void PartitionForest_THIS::merge_tree_roots(int u, int v)
{
	// Step 1:	Lookup the actual indices of the roots we want to merge.
	u = lookup_branch_index(NodeHandle(highest_layer(), u));
	v = lookup_branch_index(NodeHandle(highest_layer(), v));

	// Step 2:	Check that the roots are adjacent to each other.
	BranchLayer& rootLayer = *m_branchLayers[m_branchLayers.size()-1];
	if(!rootLayer.has_edge(u,v)) throw Exception("The tree roots cannot be merged because they are not adjacent");

	// Step 3:	Remove the edge between them.
	rootLayer.remove_edge(u,v);

	// Step 4:	Order the roots by index.
	int smaller = u <= v ? u : v;
	int larger = u <= v ? v : u;

	// Step 5:	Move the children of the larger root to the smaller root and recalculate the properties.
	Branch& smallerRoot = rootLayer(smaller);
	Branch& largerRoot = rootLayer(larger);
	for(std::set<int>::const_iterator it=largerRoot.m_children.begin(), iend=largerRoot.m_children.end(); it!=iend; ++it)
	{
		smallerRoot.m_children.insert(*it);		

		Node& child = const_cast<Node&>(get_node(NodeHandle(highest_layer()-1, *it)));
		child.m_parent = smaller;
	}
	recalculate_properties(NodeHandle(highest_layer(), smaller));

	// Step 6:	Cache all the edges adjacent to the larger root.
	std::vector<BranchLayer::Edge> edges;
	BranchLayer::EdgeIterator_Ptr edgeIt = rootLayer.adjacent_edges(larger);
	while(edgeIt->has_next()) edges.push_back(edgeIt->next());

	// Step 7:	Remove the larger root from the graph.
	rootLayer.remove_node(larger);

	// Step 8:	Add all the cached edges back into the graph, replacing the index of the larger root with
	//			that of the smaller root in each case. In some cases, there may be an existing edge with
	//			the same endpoints (because the smaller and larger roots were both adjacent to a given
	//			node) - in that case, a decision must be made as to which to keep, based on the edge weights.
	for(size_t i=0, size=edges.size(); i<size; ++i)
	{
		const BranchLayer::Edge& e = edges[i];
		int eu = e.u();
		int ev = e.v();
		if(eu == larger) eu = smaller;
		if(ev == larger) ev = smaller;

		if(rootLayer.has_edge(eu,ev))
		{
			if(e.value() < rootLayer(eu,ev))	// FIXME: This condition should be externally controllable.
			{
				rootLayer.replace_edge(eu, ev, e.value());
			}
		}
		else
		{
			rootLayer.add_edge(eu, ev, e.value());
		}
	}
}

//#################### PRIVATE METHODS ####################
PartitionForest_HEADER
void PartitionForest_THIS::check_branch_layer(int layer) const
{
	if(!has_branch_layer(layer)) throw Exception(OSSWrapper() << "No such branch layer: " << layer);
}

PartitionForest_HEADER
bool PartitionForest_THIS::has_branch_layer(int layer) const
{
	int branchCount = static_cast<int>(m_branchLayers.size());
	return 1 <= layer && layer <= branchCount;
}

PartitionForest_HEADER
int PartitionForest_THIS::lookup_branch_index(const NodeHandle& nh) const
{
	check_branch_layer(nh.layer);

	Node *cur = &(*m_leafLayer)(nh.index);
	int index = nh.index;
	for(int i=1; i<=nh.layer; ++i)
	{
		int parent = cur->parent();
		if(parent != -1)
		{
			cur = &(*m_branchLayers[i-1])(parent);
			index = parent;
		}
		else throw Exception(OSSWrapper() << "Lookup of branch (" << nh.layer << ',' << nh.index << ") failed");
	}
	return index;
}

PartitionForest_HEADER
void PartitionForest_THIS::recalculate_properties(const NodeHandle& nh)
{
	assert(nh.layer >= 1);

	Branch& parent = const_cast<Branch&>(get_branch(nh));
	const std::set<int>& children = parent.children();

	if(nh.layer == 1)
	{
		// Recalculate the node's properties from the leaves below it.
		const LeafLayer& childLayer = *m_leafLayer;
		std::vector<LeafProperties> childProperties;
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			const Leaf& child = childLayer(*it);
			childProperties.push_back(child.m_properties);
		}
		parent.m_properties = BranchProperties::combine(childProperties);
	}
	else
	{
		// Recalculate the node's properties from the branches below it.
		const BranchLayer& childLayer = *m_branchLayers[nh.layer-2];
		std::vector<BranchProperties> childProperties;
		for(std::set<int>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			const Branch& child = childLayer(*it);
			childProperties.push_back(child.m_properties);
		}
		parent.m_properties = BranchProperties::combine(childProperties);
	}
}

}

#undef PartitionForest_HEADER
#undef PartitinoForest_THIS
