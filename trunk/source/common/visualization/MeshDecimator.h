/***
 * millipede: MeshDecimator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHDECIMATOR
#define H_MILLIPEDE_MESHDECIMATOR

#include <common/adts/PriorityQueue.h>
#include "MeshTransformer.h"
#include "MeshUtil.h"
#include "SimpleMeshNodeDecimator.h"

namespace mp {

template <typename Label>
class MeshDecimator : public MeshTransformer<Label>
{
	//#################### TYPEDEFS ####################
private:
	typedef Mesh<Label> MeshT;
	typedef boost::shared_ptr<MeshT> Mesh_Ptr;
	typedef MeshNodeDecimator<Label> MeshNodeDecimatorT;
	typedef boost::shared_ptr<MeshNodeDecimatorT> MeshNodeDecimator_Ptr;
	typedef MeshNode<Label> MeshNodeT;
	typedef std::vector<MeshNodeT> MeshNodeVector;
	typedef MeshTriangle<Label> MeshTriangleT;
	typedef std::list<MeshTriangleT> MeshTriangleList;
	typedef PriorityQueue<int, double, MeshNodeDecimator_Ptr> PriQ;
	typedef SimpleMeshNodeDecimator<Label> SimpleMeshNodeDecimatorT;

	typedef std::map<int,std::vector<MeshTriangleT> > AdjacentTriangleMap;

	//#################### PRIVATE VARIABLES ####################
private:
	AdjacentTriangleMap m_adjacentTriangleMap;	///< a map from the mesh nodes to the triangles surrounding them
	int m_reductionTarget;						///< the percentage of triangles to try and remove - in the range [0,100] (obviously)

	//#################### CONSTRUCTORS ####################
public:
	explicit MeshDecimator(int reductionTarget)
	:	m_reductionTarget(reductionTarget)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void construct_adjacent_triangle_map(const Mesh_Ptr& mesh)
	{
		const MeshTriangleList& triangles = mesh->triangles();
		for(typename MeshTriangleList::const_iterator it=triangles.begin(), iend=triangles.end(); it!=iend; ++it)
		{
			for(int j=0; j<3; ++j)
			{
				m_adjacentTriangleMap[it->index(j)].push_back(*it);
			}
		}
	}

	void construct_priority_queue(PriQ& pq, const Mesh_Ptr& mesh) const
	{
		const MeshNodeVector& nodes = mesh->nodes();
		int nodeCount = static_cast<int>(nodes.size());
		for(int i=0; i<nodeCount; ++i)
		{
			switch(MeshUtil::classify_node(i, nodes))
			{
				case MeshNodeType::SIMPLE:
				{
					MeshNodeDecimator_Ptr nodeDecimator(new SimpleMeshNodeDecimatorT(i, mesh, m_adjacentTriangleMap.find(i)->second));
					if(nodeDecimator->valid()) pq.insert(i, nodeDecimator->metric(), nodeDecimator);
					break;
				}
				case MeshNodeType::EDGE:
				{
					// TODO: Implement the decimation scheme for edge nodes.
					break;
				}
				case MeshNodeType::CORNER:
				{
					// Corner nodes should not be decimated.
					break;
				}
			}
		}
	}

	void execute_impl()
	{
		Mesh_Ptr mesh = get_mesh();
		construct_adjacent_triangle_map(mesh);

		PriQ pq;
		construct_priority_queue(pq, mesh);

		int trisToRemove = mesh->triangles().size() * m_reductionTarget / 100;
		int trisRemoved = 0;

		// TODO
	}
};

}

#endif
