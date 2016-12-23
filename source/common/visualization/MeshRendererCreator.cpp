/***
 * millipede: MeshRendererCreator.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MeshRendererCreator.h"

namespace mp {

//#################### CONSTRUCTORS ####################
MeshRendererCreator::MeshRendererCreator(const DataHook<Mesh_Ptr>& meshHook, const boost::optional<std::map<int,RGBA32> >& submeshColourMap,
										 const boost::optional<std::map<std::string,int> >& submeshNameMap)
:	m_meshHook(meshHook), m_submeshColourMap(submeshColourMap), m_submeshNameMap(submeshNameMap)
{}

//#################### PUBLIC METHODS ####################
const MeshRenderer_Ptr& MeshRendererCreator::get_mesh_renderer() const
{
	return m_meshRenderer;
}

int MeshRendererCreator::length() const
{
	return 100;		// creating the mesh renderer can take substantial time for a large mesh
}

//#################### PRIVATE METHODS ####################
void MeshRendererCreator::execute_impl()
{
	set_status("Creating mesh renderer...");
	m_meshRenderer.reset(new MeshRenderer(m_meshHook.get(), m_submeshColourMap, m_submeshNameMap));
}

}
