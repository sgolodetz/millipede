#include <iostream>

#include "MinimumSpanningTree.h"
#include "PartitionForest.h"
using namespace mp;

struct Voxel
{
	int x, y, z;

	Voxel(int x_, int y_, int z_)
	:	x(x_), y(y_), z(z_)
	{}
};

class VoxelIDConverter
{
public:
	typedef Voxel Actual;

private:
	int m_xSize, m_ySize, m_zSize;

public:
	VoxelIDConverter(int xSize, int ySize, int zSize)
	:	m_xSize(xSize), m_ySize(ySize), m_zSize(zSize)
	{}

public:
	int from_actual(const Voxel& rhs) const
	{
		// NYI
		throw 23;
	}

	Voxel to_actual(int rhs) const
	{
		// NYI
		throw 23;
	}
};

struct VoxelProperties
{
	int greyValue;

	VoxelProperties(int greyValue_) : greyValue(greyValue_) {}
};

struct RegionProperties
{
	int area;
	double meanGreyValue;

	RegionProperties() : area(0), meanGreyValue(0) {}
	explicit RegionProperties(int area_, double meanGreyValue_) : area(area_), meanGreyValue(meanGreyValue_) {}

	static RegionProperties combine(const std::vector<RegionProperties>& childProperties)
	{
		// Sample implementation

		if(childProperties.empty()) /* NYI */ throw 23;

		int areaSum = 0;
		double greyValueSum = 0;
		for(size_t i=0, size=childProperties.size(); i<size; ++i)
		{
			areaSum += childProperties[i].area;
			greyValueSum += childProperties[i].area * childProperties[i].meanGreyValue;
		}
		return RegionProperties(areaSum, greyValueSum / areaSum);
	}

	static RegionProperties combine(const std::vector<VoxelProperties>& childProperties)
	{
		// Sample implementation

		if(childProperties.empty()) /* NYI */ throw 23;

		double sum = 0;
		for(size_t i=0, size=childProperties.size(); i<size; ++i)
		{
			sum += childProperties[i].greyValue;
		}
		return RegionProperties((int)childProperties.size(), sum / childProperties.size());
	}
};

typedef PartitionForest<RegionProperties, VoxelProperties, VoxelIDConverter> IPF;
typedef IPF::Branch Branch;
typedef shared_ptr<Branch> Branch_Ptr;
typedef IPF::BranchLayer BranchLayer;
typedef IPF::BranchLayer_Ptr BranchLayer_Ptr;
typedef IPF::Leaf Leaf;
typedef IPF::LeafLayer LeafLayer;
typedef IPF::LeafLayer_Ptr LeafLayer_Ptr;
typedef IPF::NodeHandle NodeHandle;

typedef MinimumSpanningTree<Branch,int> MST;
typedef shared_ptr<MST> MST_Ptr;

class VoxelGraphHigherValue : public LeafLayer
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<Leaf> m_nodes;
	int m_xSize, m_ySize, m_zSize;

	//#################### CONSTRUCTORS ####################
public:
	explicit VoxelGraphHigherValue(const std::vector<VoxelProperties>& voxels, int xSize, int ySize, int zSize)
	:	m_xSize(xSize), m_ySize(ySize), m_zSize(zSize)
	{
		size_t voxelCount = voxels.size();
		m_nodes.reserve(voxelCount);
		for(size_t i=0; i<voxelCount; ++i)
		{
			m_nodes.push_back(Leaf(voxels[i]));
		}
	}

	//#################### PUBLIC OPERATORS ####################
public:
	Leaf& operator()(int n)
	{
		check_node(n);
		return m_nodes[n];
	}

	const Leaf& operator()(int n) const
	{
		check_node(n);
		return m_nodes[n];
	}

	int operator()(int u, int v) const
	{
		if(!has_edge(u,v)) throw Exception(OSSWrapper() << "No edge exists between " << u << " and " << v);
		return std::max(m_nodes[u].properties().greyValue, m_nodes[v].properties().greyValue);
	}

	//#################### PUBLIC METHODS ####################
public:
	EdgeIterator_Ptr adjacent_edges(int n) const	{ /* NYI */ throw 23; }
	EdgeIterator_Ptr edges() const					{ /* NYI */ throw 23; }

	bool has_edge(int u, int v) const
	{
		// NYI
		return true;
	}

	bool has_node(int n) const
	{
		return 0 <= n && n < static_cast<int>(m_nodes.size());
	}

	NodeCIterator_Ptr nodes() const					{ /* NYI */ throw 23; }
};

void add_branch_node(const BranchLayer_Ptr& layer, const std::set<int>& children)
{
	if(children.empty()) throw Exception("Every branch node must have children");
	layer->add_node(*children.begin(), Branch_Ptr(new Branch(children)));
}

int main()
try
{
	std::vector<VoxelProperties> voxels;
	voxels.push_back(23);	voxels.push_back(1);	voxels.push_back(2);
	voxels.push_back(3);	voxels.push_back(4);	voxels.push_back(5);
	voxels.push_back(6);	voxels.push_back(7);	voxels.push_back(8);
	LeafLayer_Ptr leaves(new VoxelGraphHigherValue(voxels, 3, 3, 1));

	BranchLayer_Ptr branches(new BranchLayer);
	{ int c[] = {0,1,3,4};		std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {2,5};			std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {6,7};			std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {8};			std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	branches->add_edge(0, 2, 2);
	branches->add_edge(0, 6, 6);
	branches->add_edge(2, 8, 8);
	branches->add_edge(6, 8, 8);

	MST_Ptr mst(new MST(branches));

	IPF ipf(leaves, branches);
	ipf.clone_above_layer(1);
	ipf.merge_tree_roots(2,8);
	ipf.clone_above_layer(2);
	ipf.merge_tree_roots(0,6);
	ipf.clone_above_layer(3);
	ipf.merge_tree_roots(0,2);

	const Branch& branch = ipf.get_branch(NodeHandle(1,1));
	const Leaf& leaf = ipf.get_leaf(8);

	return 0;

#if 0
	BranchLayer_Ptr graph(new BranchLayer);
	{ int c[] = {0,1,3,4};		std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(graph, children); }
	{ int c[] = {2,5};			std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(graph, children); }
	{ int c[] = {6,7};			std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(graph, children); }
	{ int c[] = {8};			std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(graph, children); }
	graph->add_edge(0, 2, 23);
	Branch& b = (*graph)(0);
	int e1 = (*graph)(2,0);
	std::cout << e1 << std::endl;
	graph->replace_edge(2, 0, 9);
	int e2 = (*graph)(0,2);
	std::cout << e2 << std::endl;
	graph->add_edge(6, 2, 84);
	graph->add_edge(6, 8, 51);
	BranchLayer::EdgeIterator_Ptr it = graph->adjacent_edges(2);
	//BranchLayer::EdgeIterator_Ptr it = graph->edges();
	while(it->has_next())
	{
		const BranchLayer::Edge& e = it->next();
		std::cout << e.u() << ' ' << e.v() << ' ' << e.value() << '\n';
	}
	BranchLayer::NodeIterator_Ptr jt = graph->nodes();
	while(jt->has_next())
	{
		std::pair<int,shared_ptr<Branch> > n = jt->next();
		std::cout << n.first << '\n';
	}

	typedef MinimumSpanningTree<Branch,int> MST;
	MST mst(graph);

	return 0;
#endif
}
catch(Exception& e)
{
	std::cerr << e.cause() << std::endl;
}
