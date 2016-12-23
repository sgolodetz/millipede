/***
 * millipede: VoxelGraphHigherValue.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOXELGRAPHHIGHERVALUE
#define H_MILLIPEDE_VOXELGRAPHHIGHERVALUE

#include "PartitionForest.h"
#include "RegionProperties.h"
#include "VoxelIDConverter.h"
#include "VoxelProperties.h"

namespace mp {

class VoxelGraphHigherValue : public PartitionForest<RegionProperties, VoxelProperties, VoxelIDConverter>::LeafLayer
{
	//#################### TYPEDEFS ####################
private:
	typedef PartitionForest<RegionProperties, VoxelProperties, VoxelIDConverter> IPF;
	typedef IPF::Leaf Leaf;

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

}

#endif
