/***
 * millipede: MeshRenderer.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <cassert>
#include <climits>

#include "MeshRenderer.h"
#include "MeshUtil.h"

namespace mp {

//#################### CONSTRUCTORS ####################
MeshRenderer::MeshRenderer(const Mesh_CPtr& mesh, const boost::optional<std::map<int,RGBA32> >& submeshColourMap, const boost::optional<std::map<std::string,int> >& submeshNameMap)
:	m_wireframeEnabled(false)
{
	if(submeshColourMap) m_submeshColourMap = *submeshColourMap;
	if(submeshNameMap) m_submeshNameMap = *submeshNameMap;

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
	//			Calculate the mesh bounds while we're doing this (it's a convenient place to do it).
	m_meshLowerBound = Vector3d(INT_MAX, INT_MAX, INT_MAX);
	m_meshUpperBound = Vector3d(INT_MIN, INT_MIN, INT_MIN);

	std::vector<std::map<int,int> > clonedNodeIndices(nodes.size());
	for(size_t i=0, size=nodes.size(); i<size; ++i)
	{
		const Vector3d& pos = nodes[i].position();
		m_meshLowerBound.x = std::min(m_meshLowerBound.x, pos.x);	m_meshUpperBound.x = std::max(m_meshUpperBound.x, pos.x);
		m_meshLowerBound.y = std::min(m_meshLowerBound.y, pos.y);	m_meshUpperBound.y = std::max(m_meshUpperBound.y, pos.y);
		m_meshLowerBound.z = std::min(m_meshLowerBound.z, pos.z);	m_meshUpperBound.z = std::max(m_meshUpperBound.z, pos.z);

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
bool MeshRenderer::has_submesh(const std::string& submeshName) const
{
	std::map<std::string,int>::const_iterator it = m_submeshNameMap.find(submeshName);
	if(it == m_submeshNameMap.end()) return false;

	std::map<int,Submesh_Ptr>::const_iterator jt = m_submeshes.find(it->second);
	return jt != m_submeshes.end();
}

bool MeshRenderer::mesh_is_empty() const
{
	return m_submeshes.empty();
}

const Vector3d& MeshRenderer::mesh_lower_bound() const
{
	return m_meshLowerBound;
}

const Vector3d& MeshRenderer::mesh_upper_bound() const
{
	return m_meshUpperBound;
}

void MeshRenderer::render() const
{
	RGBA32 defaultColours[] =
	{
		ITKImageUtil::make_rgba32(0, 255, 0, 255),
		ITKImageUtil::make_rgba32(0, 0, 255, 255),
		ITKImageUtil::make_rgba32(255, 255, 0, 255),
		ITKImageUtil::make_rgba32(255, 0, 255, 255),
		ITKImageUtil::make_rgba32(0, 255, 255, 255),
		ITKImageUtil::make_rgba32(255, 255, 255, 255),
	};

	const int defaultColourCount = sizeof(defaultColours) / sizeof(RGBA32);
	int currentDefaultColour = 0;

	for(std::map<int,Submesh_Ptr>::const_iterator it=m_submeshes.begin(), iend=m_submeshes.end(); it!=iend; ++it)
	{
		RGBA32 colour;
		std::map<int,RGBA32>::const_iterator jt = m_submeshColourMap.find(it->first);
		if(jt != m_submeshColourMap.end())
		{
			colour = jt->second;
		}
		else
		{
			colour = defaultColours[currentDefaultColour];
			currentDefaultColour = (currentDefaultColour + 1) % defaultColourCount;
		}

		if(it->second->enabled)
		{
			if(m_wireframeEnabled)	render_submesh_wireframe(*it->second, colour);
			else					render_submesh_solid(*it->second, colour);
		}
	}
}

void MeshRenderer::set_submesh_enabled(const std::string& submeshName, bool submeshEnabled)
{
	std::map<std::string,int>::const_iterator it = m_submeshNameMap.find(submeshName);
	if(it == m_submeshNameMap.end()) return;

	std::map<int,Submesh_Ptr>::const_iterator jt = m_submeshes.find(it->second);
	if(jt == m_submeshes.end()) return;

	jt->second->enabled = submeshEnabled;
}

void MeshRenderer::set_wireframe_enabled(bool wireframeEnabled)
{
	m_wireframeEnabled = wireframeEnabled;
}

bool MeshRenderer::submesh_enabled(const std::string& submeshName) const
{
	std::map<std::string,int>::const_iterator it = m_submeshNameMap.find(submeshName);
	if(it == m_submeshNameMap.end()) return false;

	std::map<int,Submesh_Ptr>::const_iterator jt = m_submeshes.find(it->second);
	if(jt == m_submeshes.end()) return false;

	return jt->second->enabled;
}

std::vector<std::string> MeshRenderer::submesh_names() const
{
	std::vector<std::string> names;
	names.reserve(m_submeshNameMap.size());
	for(std::map<std::string,int>::const_iterator it=m_submeshNameMap.begin(), iend=m_submeshNameMap.end(); it!=iend; ++it)
	{
		names.push_back(it->first);
	}
	return names;
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

	float ambient[] = {0.3f, 0.3f, 0.3f, 1.0f};
	float diffuse[] = {1.0f, 1.0f, 1.0f, 1.0f};
	float position[] = {-1.0f, -1.0f, -1.0f, 0.0f};		// note: this is a directional light

	glLightfv(GL_LIGHT0, GL_AMBIENT, ambient);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuse);
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
