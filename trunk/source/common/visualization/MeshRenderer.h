/***
 * millipede: MeshRenderer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHRENDERER
#define H_MILLIPEDE_MESHRENDERER

#include <map>

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
	std::map<int,Submesh_Ptr> m_submeshes;
	bool m_wireframe;

	//#################### CONSTRUCTORS ####################
public:
	explicit MeshRenderer(const Mesh_CPtr& mesh);

	//#################### PUBLIC METHODS ####################
public:
	void render() const;
	void set_wireframe(bool wireframe);

	//#################### PRIVATE METHODS ####################
private:
	void render_submesh_wireframe(const Submesh& submesh, const RGBA32& colour) const;
};

}

#endif
