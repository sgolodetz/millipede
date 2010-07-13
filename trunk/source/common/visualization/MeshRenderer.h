/***
 * millipede: MeshRenderer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHRENDERER
#define H_MILLIPEDE_MESHRENDERER

#include <map>

#include <common/ogl/WrappedGL.h>

#include "Mesh.h"

namespace mp {

class MeshRenderer
{
	//#################### NESTED CLASSES ####################
private:
	struct GLArrays
	{
		std::vector<GLuint> indexArray;		// consecutive triples of GLuints which reference vertices in the vertex array to form triangles
		std::vector<double> normalArray;	// consecutive triples of doubles which form vertex normals (one normal per vertex)
		std::vector<double> vertexArray;	// consecutive triples of doubles which form vertices
	};

	//#################### TYPEDEFS ####################
private:
	typedef boost::shared_ptr<GLArrays> GLArrays_Ptr;
	typedef Mesh<int> MeshT;
	typedef boost::shared_ptr<const MeshT> Mesh_CPtr;
	typedef MeshNode<int> MeshNodeT;
	typedef MeshTriangle<int> MeshTriangleT;

	//#################### PRIVATE VARIABLES ####################
private:
	std::map<int,GLArrays_Ptr> m_arraysMap;
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
	void render_wireframe() const;
};

}

#endif
