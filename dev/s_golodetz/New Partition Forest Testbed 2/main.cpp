#include <iostream>

#include "MinimumSpanningTree.h"
#include "PartitionForest.h"
#include "PFWaterfallEdge.h"
#include "Waterfall.h"
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
typedef shared_ptr<IPF> IPF_Ptr;
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

typedef PFWaterfallEdge<RegionProperties, VoxelProperties, VoxelIDConverter> PFWE;
typedef shared_ptr<PFWE> PFWE_Ptr;

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
	NodeCIterator_Ptr adjacent_nodes(int n) const	{ /* NYI */ throw 23; }
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

	int node_count() const							{ /* NYI */ throw 23; }
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
	std::vector<VoxelProperties> voxels(56, 0);
	LeafLayer_Ptr leaves(new VoxelGraphHigherValue(voxels, 7, 8, 1));

	BranchLayer_Ptr branches(new BranchLayer);
	{ int c[] = {0,1,2,7,8,9};						std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {3,4,5,10,11,12,13};				std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {6};								std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {14,21,28,29,35,36,37,42,43,44};	std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {15,16,17,22,23,30};				std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {18,19,26,33};						std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {20,27,34,41};						std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {24,25};							std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {31,38};							std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {32,39,46};							std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {40,47,48,54,55};					std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {45,52};							std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {49,50,51};							std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	{ int c[] = {53};								std::set<int> children(c, c+sizeof(c)/sizeof(int));		add_branch_node(branches, children); }
	branches->add_edge(0, 3, 3);
	branches->add_edge(3, 6, 2);
	branches->add_edge(3, 15, 20);
	branches->add_edge(14, 15, 4);
	branches->add_edge(14, 49, 2);
	branches->add_edge(15, 24, 10);
	branches->add_edge(18, 20, 4);
	branches->add_edge(18, 32, 20);
	branches->add_edge(18, 40, 4);
	branches->add_edge(24, 31, 5);
	branches->add_edge(31, 32, 5);
	branches->add_edge(32, 45, 4);
	branches->add_edge(32, 53, 2);

	IPF_Ptr ipf(new IPF(leaves, branches));
	MST mst(ipf->get_branch_layer(1));
	PFWE_Ptr waterfallTree = PFWE::construct_waterfall_tree(mst, ipf);

	while(ipf->get_branch_layer(ipf->highest_layer()).node_count() > 1)
	{
		ipf->clone_above_layer(ipf->highest_layer());
		Waterfall::iterate(waterfallTree);
	}

	return 0;
}
catch(Exception& e)
{
	std::cerr << e.cause() << std::endl;
}
