/***
 * millipede: MeshTransformer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHTRANSFORMER
#define H_MILLIPEDE_MESHTRANSFORMER

#include <common/jobs/DataHook.h>
#include <common/jobs/SimpleJob.h>
#include "Mesh.h"

namespace mp {

template <typename Label>
class MeshTransformer : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef Mesh<Label> MeshT;
	typedef boost::shared_ptr<MeshT> Mesh_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	DataHook<Mesh_Ptr> m_meshHook;

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

	void set_mesh(const Mesh_Ptr& mesh)
	{
		m_meshHook.set(mesh);
	}

	void set_mesh_hook(const DataHook<Mesh_Ptr>& meshHook)
	{
		m_meshHook = meshHook;
	}
};

}

#endif
