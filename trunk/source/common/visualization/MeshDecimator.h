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
	void clean_triangle_list(const Mesh_Ptr& mesh) const
	{
		const MeshNodeVector& nodes = mesh->nodes();
		MeshTriangleList& triangles = mesh->triangles();
		for(typename MeshTriangleList::const_iterator it=triangles.begin(), iend=triangles.end(); it!=iend; /* no-op */)
		{
			int i0 = it->index(0), i1 = it->index(1), i2 = it->index(2);
			if(!nodes[i0].valid() || !nodes[i1].valid() || !nodes[i2].valid())
			{
				it = triangles.erase(it);
			}
			else ++it;
		}
	}

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

		while(!pq.empty() && trisRemoved < trisToRemove)
		{
			PriQ::Element e = pq.top();
			pq.pop();
			MeshTriangleList tris = e.data()->decimate();

			int triCount = static_cast<int>(tris.size());
			if(triCount > 0)
			{
				int index = e.data()->index();
				MeshNodeVector& nodes = mesh->nodes();
				MeshNodeT& n = nodes[index];
				trisRemoved += n.adjacent_node_count() - triCount;

				// Invalidate the node and remove references to it from the surrounding nodes.
				n.invalidate();
				for(std::set<int>::const_iterator it=n.adjacent_nodes().begin(), iend=n.adjacent_nodes().end(); it!=iend; ++it)
				{
					nodes[*it].remove_adjacent_node(index);
				}

				// Add any new edges introduced by the retriangulation.
				for(typename MeshTriangleList::const_iterator it=tris.begin(), iend=tris.end(); it!=iend; ++it)
				{
					int i0 = it->index(0), i1 = it->index(1), i2 = it->index(2);
					MeshNodeT& n0 = nodes[i0];
					MeshNodeT& n1 = nodes[i1];
					MeshNodeT& n2 = nodes[i2];
					n0.add_adjacent_node(i1);	n0.add_adjacent_node(i2);
					n1.add_adjacent_node(i0);	n1.add_adjacent_node(i2);
					n2.add_adjacent_node(i0);	n2.add_adjacent_node(i1);
				}

				// Splice the new triangles onto the end of the triangle list.
				MeshTriangleList& meshTriangles = mesh->triangles();
				meshTriangles.splice(meshTriangles.end(), tris);

				// Recalculate the metrics for the surrounding nodes and update their keys in the priority queue.
				for(std::set<int>::const_iterator it=n.adjacent_nodes().begin(), iend=n.adjacent_nodes().end(); it!=iend; ++it)
				{
					if(pq.contains(*it))
					{
						PriQ::Element& adj = pq.element(*it);
						adj.data()->calculate_details();
						if(adj.data()->valid()) pq.update_key(*it, adj.data()->metric());
						else pq.erase(*it);
					}
				}
			}
		}

		clean_triangle_list(mesh);
		rebuild_node_array(mesh);
	}

	void rebuild_node_array(const Mesh_Ptr& mesh) const
	{
		MeshNodeVector& nodes = mesh->nodes();
		MeshTriangleList& triangles = mesh->triangles();
		size_t nodeCount = nodes.size();

		// Rebuild the node array.
		std::vector<int> mapping(nodeCount, -1);
		MeshNodeVector newNodes;
		for(size_t i=0; i<nodeCount; ++i)
		{
			if(nodes[i].valid())
			{
				mapping[i] = static_cast<int>(newNodes.size());
				newNodes.push_back(nodes[i]);
			}
		}
		nodes = newNodes;

		// Update the indices in the adjacent node sets using the mapping.
		nodeCount = nodes.size();
		for(size_t i=0; i<nodeCount; ++i)
		{
			// Note: I'm using a vector (rather than a set) here because you can't update values in a set.
			std::vector<int> adjacentNodes(nodes[i].adjacent_nodes().begin(), nodes[i].adjacent_nodes().end());
			for(std::vector<int>::iterator jt=adjacentNodes.begin(), jend=adjacentNodes.end(); jt!=jend; ++jt)
			{
				*jt = mapping[*jt];
			}
			nodes[i].set_adjacent_nodes(std::set<int>(adjacentNodes.begin(), adjacentNodes.end()));
		}

		// Update the indices in the triangle list using the mapping.
		for(typename MeshTriangleList::iterator it=triangles.begin(), iend=triangles.end(); it!=iend; ++it)
		{
			for(int j=0; j<3; ++j)
			{
				it->set_index(j, mapping[it->index(j)]);
			}
		}
	}
};

}

#endif
