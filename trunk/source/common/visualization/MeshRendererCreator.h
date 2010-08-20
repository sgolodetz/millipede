/***
 * millipede: MeshRendererCreator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHRENDERERCREATOR
#define H_MILLIPEDE_MESHRENDERERCREATOR

#include <common/jobs/DataHook.h>
#include <common/jobs/SimpleJob.h>
#include "MeshRenderer.h"

namespace mp {

class MeshRendererCreator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef boost::shared_ptr<Mesh<int> > Mesh_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	DataHook<Mesh_Ptr> m_meshHook;
	MeshRenderer_Ptr m_meshRenderer;
	std::map<int,RGBA32> m_submeshColourMap;
	std::map<std::string,int> m_submeshNameMap;

	//#################### CONSTRUCTORS ####################
public:
	MeshRendererCreator(const DataHook<Mesh_Ptr>& meshHook, const std::map<int,RGBA32>& submeshColourMap, const std::map<std::string,int>& submeshNameMap);

	//#################### PUBLIC METHODS ####################
public:
	const MeshRenderer_Ptr& get_mesh_renderer() const;
	int length() const;

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl();
};

}

#endif
