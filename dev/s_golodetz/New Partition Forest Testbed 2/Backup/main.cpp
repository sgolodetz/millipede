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
	double meanGreyValue;

	RegionProperties() : meanGreyValue(0) {}
	explicit RegionProperties(double meanGreyValue_) : meanGreyValue(meanGreyValue_) {}

	static RegionProperties combine(const std::vector<RegionProperties>& childProperties)
	{
		// NYI
		throw 23;
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
		return RegionProperties(sum / childProperties.size());
	}
};

typedef PartitionForest<RegionProperties, VoxelProperties, VoxelIDConverter> IPF;
typedef IPF::Branch Branch;
typedef shared_ptr<Branch> Branch_Ptr;
typedef IPF::BranchLayerGraph BranchLayerGraph;
typedef IPF::BranchLayerGraph_Ptr BranchLayerGraph_Ptr;

void add_branch_node(const BranchLayerGraph_Ptr& graph, const std::set<int>& children)
{
	if(children.empty()) throw Exception("Every branch node must have children");
	graph->add_node(*children.begin(), Branch_Ptr(new Branch(children)));
}

int main()
try
{
	BranchLayerGraph_Ptr graph(new BranchLayerGraph);
	{ int c[] = {0,1,3,4};		std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(graph, children); }
	{ int c[] = {2,5};			std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(graph, children); }
	{ int c[] = {6,7};			std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(graph, children); }
	{ int c[] = {8};			std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(graph, children); }
	graph->add_edge(0, 2, 23);
	Branch& b = (*graph)(0);
	int& e1 = (*graph)(2,0);
	std::cout << e1 << std::endl;
	e1 = 9;
	int& e2 = (*graph)(0,2);
	std::cout << e2 << std::endl;
	graph->add_edge(6, 2, 84);
	graph->add_edge(6, 8, 51);
	BranchLayerGraph::EdgeCRefIterator_Ptr it = graph->adjacent_edges(2);
	//BranchLayerGraph::EdgeCRefIterator_Ptr it = graph->edges();
	while(it->has_next())
	{
		const BranchLayerGraph::Edge& e = it->next();
		std::cout << e.u() << ' ' << e.v() << ' ' << e.value() << '\n';
	}
	BranchLayerGraph::NodeRefIterator_Ptr jt = graph->nodes();
	while(jt->has_next())
	{
		std::pair<int,shared_ptr<Branch> > n = jt->next();
		std::cout << n.first << '\n';
	}

	typedef MinimumSpanningTree<Branch,int> MST;
	MST mst(graph);

	return 0;
}
catch(Exception& e)
{
	std::cerr << e.cause() << std::endl;
}
