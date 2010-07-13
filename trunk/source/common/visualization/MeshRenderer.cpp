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
:	m_wireframe(true)
{
	const std::vector<MeshNodeT>& nodes = mesh->nodes();
	const std::list<MeshTriangleT>& triangles = mesh->triangles();

	// Step 1:	Set up the arrays for each label used.
	for(std::vector<MeshNodeT>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		std::set<int> labels = it->labels();
		for(std::set<int>::const_iterator jt=labels.begin(), jend=labels.end(); jt!=jend; ++jt)
		{
			GLArrays_Ptr& arrays = m_arraysMap[*jt];
			if(!arrays) arrays.reset(new GLArrays);
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
			GLArrays& arrays = *m_arraysMap[*jt];
			clonedNodeIndices[i].insert(std::make_pair(*jt, arrays.vertexArray.size() / 3));

			arrays.vertexArray.push_back(pos.x);
			arrays.vertexArray.push_back(pos.y);
			arrays.vertexArray.push_back(pos.z);
		}
	}

	// Step 3:	Prepare the normal array for each label.
	for(std::map<int,GLArrays_Ptr>::const_iterator it=m_arraysMap.begin(), iend=m_arraysMap.end(); it!=iend; ++it)
	{
		it->second->normalArray.resize(it->second->vertexArray.size());
	}

	// Step 4:	Run through the triangles, filling in the index array and adding to the values in the normal array for each label.
	for(std::list<MeshTriangleT>::const_iterator it=triangles.begin(), iend=triangles.end(); it!=iend; ++it)
	{
		int indices[3] = {it->index(0), it->index(1), it->index(2)};

		// The length of this is twice the triangle's area: 
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

			GLArrays& arrays = *m_arraysMap[label];
			if(j == 0)	// the winding stored in the triangle is correct for the first label...
			{
				arrays.indexArray.push_back(clones[0]);
				arrays.indexArray.push_back(clones[1]);
				arrays.indexArray.push_back(clones[2]);

				for(int k=0; k<3; ++k)
				{
					int normalOffset = clones[k] * 3;
					arrays.normalArray[normalOffset]   += normal.x;
					arrays.normalArray[normalOffset+1] += normal.y;
					arrays.normalArray[normalOffset+2] += normal.z;
				}
			}
			else		// ...but reversed for the second label
			{
				arrays.indexArray.push_back(clones[1]);
				arrays.indexArray.push_back(clones[0]);
				arrays.indexArray.push_back(clones[2]);

				for(int k=0; k<3; ++k)
				{
					int normalOffset = clones[k] * 3;
					arrays.normalArray[normalOffset]   -= normal.x;
					arrays.normalArray[normalOffset+1] -= normal.y;
					arrays.normalArray[normalOffset+2] -= normal.z;
				}
			}
		}
	}

	// Step 5:	Normalize all the normals in all the normal arrays (except for those which are too close to zero to normalize).
	for(std::map<int,GLArrays_Ptr>::const_iterator it=m_arraysMap.begin(), iend=m_arraysMap.end(); it!=iend; ++it)
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
	// TODO
}

void MeshRenderer::set_wireframe(bool wireframe)
{
	m_wireframe = wireframe;
}

//#################### PRIVATE METHODS ####################
void MeshRenderer::render_wireframe() const
{
	// TODO
}

}
