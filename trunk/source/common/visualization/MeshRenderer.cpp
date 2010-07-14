/***
 * millipede: MeshRenderer.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <cassert>

#include "MeshRenderer.h"
#include "MeshUtil.h"

namespace mp {

//#################### CONSTRUCTORS ####################
MeshRenderer::MeshRenderer(const Mesh_CPtr& mesh)
:	m_wireframe(false)
{
	const std::vector<MeshNodeT>& nodes = mesh->nodes();
	const std::list<MeshTriangleT>& triangles = mesh->triangles();

	// Step 1:	Set up the submesh for each label used.
	for(std::vector<MeshNodeT>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		std::set<int> labels = it->labels();
		for(std::set<int>::const_iterator jt=labels.begin(), jend=labels.end(); jt!=jend; ++jt)
		{
			Submesh_Ptr& submesh = m_submeshes[*jt];
			if(!submesh) submesh.reset(new Submesh);
		}
	}

	// Step 2:	Clone each node for each of its labels, and store the clones in the relevant arrays (keeping a track of where they go).
	std::vector<std::map<int,int> > clonedNodeIndices(nodes.size());
	for(size_t i=0, size=nodes.size(); i<size; ++i)
	{
		const Vector3d& pos = nodes[i].position();

		std::set<int> labels = nodes[i].labels();
		for(std::set<int>::const_iterator jt=labels.begin(), jend=labels.end(); jt!=jend; ++jt)
		{
			Submesh& submesh = *m_submeshes[*jt];
			clonedNodeIndices[i].insert(std::make_pair(*jt, submesh.vertexArray.size() / 3));

			submesh.vertexArray.push_back(pos.x);
			submesh.vertexArray.push_back(pos.y);
			submesh.vertexArray.push_back(pos.z);
		}
	}

	// Step 3:	Prepare the normal array for each label.
	for(std::map<int,Submesh_Ptr>::const_iterator it=m_submeshes.begin(), iend=m_submeshes.end(); it!=iend; ++it)
	{
		it->second->normalArray.resize(it->second->vertexArray.size());
	}

	// Step 4:	Run through the triangles, filling in the index array and adding to the values in the normal array for each label.
	for(std::list<MeshTriangleT>::const_iterator it=triangles.begin(), iend=triangles.end(); it!=iend; ++it)
	{
		int indices[3] = {it->index(0), it->index(1), it->index(2)};

		// The length of this is twice the triangle's area.
		Vector3d normal = MeshUtil::calculate_unnormalized_normal(*it, nodes);

		const std::set<int>& labelSet = it->labels();
		std::vector<int> labels(labelSet.begin(), labelSet.end());
		assert(labels.size() == 2);

		for(int j=0; j<2; ++j)
		{
			const int label = labels[j];

			int clones[3];
			for(int k=0; k<3; ++k)
			{
				clones[k] = clonedNodeIndices[indices[k]][label];
			}

			Submesh& submesh = *m_submeshes[label];
			if(j == 0)	// the winding stored in the triangle is correct for the first label...
			{
				submesh.indexArray.push_back(clones[0]);
				submesh.indexArray.push_back(clones[1]);
				submesh.indexArray.push_back(clones[2]);

				for(int k=0; k<3; ++k)
				{
					int normalOffset = clones[k] * 3;
					submesh.normalArray[normalOffset]   += normal.x;
					submesh.normalArray[normalOffset+1] += normal.y;
					submesh.normalArray[normalOffset+2] += normal.z;
				}
			}
			else		// ...but reversed for the second label
			{
				submesh.indexArray.push_back(clones[1]);
				submesh.indexArray.push_back(clones[0]);
				submesh.indexArray.push_back(clones[2]);

				for(int k=0; k<3; ++k)
				{
					int normalOffset = clones[k] * 3;
					submesh.normalArray[normalOffset]   -= normal.x;
					submesh.normalArray[normalOffset+1] -= normal.y;
					submesh.normalArray[normalOffset+2] -= normal.z;
				}
			}
		}
	}

	// Step 5:	Normalize all the normals in all the normal arrays (except for those which are too close to zero to normalize).
	for(std::map<int,Submesh_Ptr>::const_iterator it=m_submeshes.begin(), iend=m_submeshes.end(); it!=iend; ++it)
	{
		std::vector<double>& normalArray = it->second->normalArray;
		for(size_t j=0, size=normalArray.size(); j<size; j+=3)
		{
			Vector3d v(normalArray[j], normalArray[j+1], normalArray[j+2]);
			if(v.length() >= MathConstants::SMALL_EPSILON)
			{
				v.normalize();
				normalArray[j]   = v.x;
				normalArray[j+1] = v.y;
				normalArray[j+2] = v.z;
			}
		}
	}
}

//#################### PUBLIC METHODS ####################
void MeshRenderer::render() const
{
	// TEMPORARY: This should be replaced with a colour mapping passed in by the caller.
	std::vector<RGBA32> colours;
	colours.push_back(ITKImageUtil::make_rgba32(255, 0, 0, 255));
	colours.push_back(ITKImageUtil::make_rgba32(0, 255, 0, 255));
	colours.push_back(ITKImageUtil::make_rgba32(0, 0, 255, 255));
	colours.push_back(ITKImageUtil::make_rgba32(255, 255, 0, 255));
	colours.push_back(ITKImageUtil::make_rgba32(255, 0, 255, 255));
	colours.push_back(ITKImageUtil::make_rgba32(0, 255, 255, 255));
	int n = 0;
	for(std::map<int,Submesh_Ptr>::const_iterator it=m_submeshes.begin(), iend=m_submeshes.end(); it!=iend; ++it)
	{
		if(it->second->enabled)
		{
			if(m_wireframe)	render_submesh_wireframe(*it->second, colours[n]);
			else			render_submesh_solid(*it->second, colours[n]);
		}
		n = (n+1) % colours.size();
	}
}

void MeshRenderer::set_wireframe(bool wireframe)
{
	m_wireframe = wireframe;
}

//#################### PRIVATE METHODS ####################
void MeshRenderer::render_submesh_solid(const Submesh& submesh, const RGBA32& colour) const
{
	if(submesh.indexArray.empty() || submesh.vertexArray.empty())
	{
		// This should never actually happen - I'm just playing it safe.
		return;
	}

	glPushAttrib(GL_ENABLE_BIT | GL_POLYGON_BIT);
	glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);

	glVertexPointer(3, GL_DOUBLE, 0, &submesh.vertexArray[0]);
	glNormalPointer(GL_DOUBLE, 0, &submesh.normalArray[0]);
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);

	// TODO: Allow the user to specify the lighting.
	glEnable(GL_LIGHTING);
	glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);

	float noAmbient[] = {0.0f, 0.0f, 0.0f, 1.0f};
	float whiteDiffuse[] = {1.0f, 1.0f, 1.0f, 1.0f};
	float position[] = {1.0f, 1.0f, 0.0f, 0.0f};		// actually specifies the direction for a directional light like this one
   
	glLightfv(GL_LIGHT0, GL_AMBIENT, noAmbient);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, whiteDiffuse);
	glLightfv(GL_LIGHT0, GL_POSITION, position);
	glEnable(GL_LIGHT0);

	glColor4ub(colour[0], colour[1], colour[2], colour[3]);
	glDrawElements(GL_TRIANGLES, static_cast<GLsizei>(submesh.indexArray.size()), GL_UNSIGNED_INT, &submesh.indexArray[0]);

	glPopClientAttrib();
	glPopAttrib();
}

void MeshRenderer::render_submesh_wireframe(const Submesh& submesh, const RGBA32& colour) const
{
	if(submesh.indexArray.empty() || submesh.vertexArray.empty())
	{
		// This should never actually happen - I'm just playing it safe.
		return;
	}

	glPushAttrib(GL_ENABLE_BIT | GL_POLYGON_BIT);
	glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);

	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

	glVertexPointer(3, GL_DOUBLE, 0, &submesh.vertexArray[0]);
	glEnableClientState(GL_VERTEX_ARRAY);

	glColor4ub(colour[0], colour[1], colour[2], colour[3]);
	glDrawElements(GL_TRIANGLES, static_cast<GLsizei>(submesh.indexArray.size()), GL_UNSIGNED_INT, &submesh.indexArray[0]);

	glPopClientAttrib();
	glPopAttrib();
}

}
