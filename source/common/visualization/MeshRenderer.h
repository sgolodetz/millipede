/***
 * millipede: MeshRenderer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHRENDERER
#define H_MILLIPEDE_MESHRENDERER

#include <map>
#include <string>

#include <boost/optional.hpp>

#include <common/ogl/WrappedGL.h>
#include <common/util/ITKImageUtil.h>
#include "Mesh.h"

namespace mp {

class MeshRenderer
{
	//#################### NESTED CLASSES ####################
private:
	struct Submesh
	{
		bool enabled;
		std::vector<GLuint> indexArray;		// consecutive triples of GLuints which reference vertices in the vertex array to form triangles
		std::vector<double> normalArray;	// consecutive triples of doubles which form vertex normals (one normal per vertex)
		std::vector<double> vertexArray;	// consecutive triples of doubles which form vertices

		Submesh()
		:	enabled(true)
		{}
	};

	//#################### TYPEDEFS ####################
private:
	typedef Mesh<int> MeshT;
	typedef boost::shared_ptr<const MeshT> Mesh_CPtr;
	typedef MeshNode<int> MeshNodeT;
	typedef MeshTriangle<int> MeshTriangleT;
	typedef boost::shared_ptr<Submesh> Submesh_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	Vector3d m_meshLowerBound, m_meshUpperBound;
	std::map<int,RGBA32> m_submeshColourMap;
	std::map<std::string,int> m_submeshNameMap;
	std::map<int,Submesh_Ptr> m_submeshes;
	bool m_wireframeEnabled;

	//#################### CONSTRUCTORS ####################
public:
	MeshRenderer(const Mesh_CPtr& mesh, const boost::optional<std::map<int,RGBA32> >& submeshColourMap = boost::none, const boost::optional<std::map<std::string,int> >& submeshNameMap = boost::none);

	//#################### PUBLIC METHODS ####################
public:
	bool has_submesh(const std::string& submeshName) const;
	bool mesh_is_empty() const;
	const Vector3d& mesh_lower_bound() const;
	const Vector3d& mesh_upper_bound() const;
	void render() const;
	void set_submesh_enabled(const std::string& submeshName, bool submeshEnabled);
	void set_wireframe_enabled(bool wireframeEnabled);
	bool submesh_enabled(const std::string& submeshName) const;
	std::vector<std::string> submesh_names() const;

	//#################### PRIVATE METHODS ####################
private:
	void render_submesh_solid(const Submesh& submesh, const RGBA32& colour) const;
	void render_submesh_wireframe(const Submesh& submesh, const RGBA32& colour) const;
};

//#################### TYPEDEFS ####################
typedef boost::shared_ptr<MeshRenderer> MeshRenderer_Ptr;

}

#endif
