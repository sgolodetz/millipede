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

namespace mp {

template <typename Label>
class LaplacianSmoother : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef Mesh<Label> MeshT;
	typedef boost::shared_ptr<MeshT> Mesh_Ptr;

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
			set_status(OSSWrapper() << "Smoothing mesh (" << i << ")...");
			iterate(mesh);
		}
	}

	void iterate(const Mesh_Ptr& mesh)
	{
		// TODO
	}
};

}

#endif
