/***
 * millipede: LaplacianSmoother.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_LAPLACIANSMOOTHER
#define H_MILLIPEDE_LAPLACIANSMOOTHER

#include <common/io/util/OSSWrapper.h>
#include <common/jobs/DataHook.h>
#include <common/jobs/SimpleJob.h>
#include "Mesh.h"
#include "MeshUtil.h"

namespace mp {

template <typename Label>
class LaplacianSmoother : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef Mesh<Label> MeshT;
	typedef boost::shared_ptr<MeshT> Mesh_Ptr;
	typedef MeshNode<Label> MeshNodeT;
	typedef std::vector<MeshNodeT> MeshNodeVector;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_iterations;	///< the number of smoothing iterations to perform
	double m_lambda;

	DataHook<Mesh_Ptr> m_meshHook;

	//#################### CONSTRUCTORS ####################
public:
	LaplacianSmoother(double lambda = 0.5, int iterations = 6)
	:	m_iterations(iterations), m_lambda(lambda)
	{}

	//#################### PUBLIC METHODS ####################
public:
	const Mesh_Ptr& get_mesh() const
	{
		return m_meshHook.get();
	}

	const DataHook<Mesh_Ptr>& get_mesh_hook() const
	{
		return m_meshHook;
	}

	int length() const
	{
		return m_iterations;
	}

	void set_mesh(const Mesh_Ptr& mesh)
	{
		m_meshHook.set(mesh);
	}

	void set_mesh_hook(const DataHook<Mesh_Ptr>& meshHook)
	{
		m_meshHook = meshHook;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl()
	{
		Mesh_Ptr mesh = m_meshHook.get();
		for(int i=0; i<m_iterations; ++i)
		{
			set_status(OSSWrapper() << "Smoothing mesh (iteration " << i << ")...");
			iterate(mesh);
		}
	}

	void iterate(const Mesh_Ptr& mesh)
	{
		MeshNodeVector& nodes = mesh->nodes();
		int nodeCount = static_cast<int>(nodes.size());

		// Calculate the new node positions. Note that they have to be stored separately since we need
		// the old node positions in order to calculate them.
		std::vector<Vector3d> newPositions(nodeCount);
		for(int i=0; i<nodeCount; ++i)
		{
			newPositions[i] = nodes[i].position();

			std::set<int> neighbours;								// holds the neighbours which might affect a node (depends on the node type)
			switch(MeshUtil::classify_node(i, nodes, neighbours))	// the type of node affects how the node is allowed to move
			{
				case MeshNodeType::SIMPLE:
				case MeshNodeType::EDGE:
				{
					for(std::set<int>::const_iterator jt=neighbours.begin(), jend=neighbours.end(); jt!=jend; ++jt)
					{
						Vector3d offset = nodes[*jt].position() - nodes[i].position();
						offset *= m_lambda / neighbours.size();
						newPositions[i] += offset;
					}
					break;
				}
				case MeshNodeType::CORNER:
				{
					// Corner nodes are topologically important and must stay fixed.
					break;
				}
			}
		}

		// Copy them across to the mesh.
		for(int i=0; i<nodeCount; ++i)
		{
			nodes[i].set_position(newPositions[i]);
		}
	}
};

}

#endif
