/***
 * millipede: CubeTable.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CubeTable.h"

#include <common/exceptions/Exception.h>

namespace mp {

//#################### PUBLIC METHODS ####################
std::vector<Vector3i> CubeTable::cube_vertices(int x, int y, int z)
{
	std::vector<Vector3i> verts;
	verts.reserve(8);
	verts.push_back(Vector3i(x,y,z));
	verts.push_back(Vector3i(x+1,y,z));
	verts.push_back(Vector3i(x,y+1,z));
	verts.push_back(Vector3i(x+1,y+1,z));
	verts.push_back(Vector3i(x,y,z+1));
	verts.push_back(Vector3i(x+1,y,z+1));
	verts.push_back(Vector3i(x,y+1,z+1));
	verts.push_back(Vector3i(x+1,y+1,z+1));
	return verts;
}

std::vector<Vector3i> CubeTable::face_vertices(int x, int y, int z, CubeFaceDesignator::Enum f)
{
	std::vector<Vector3i> verts(4);
	switch(f)
	{
		case CubeFaceDesignator::FACE_XY:
			verts[CubeFace::TOP_LEFT_VERTEX] = Vector3i(x,y+1,z);	verts[CubeFace::TOP_RIGHT_VERTEX] = Vector3i(x+1,y+1,z);
			verts[CubeFace::BOTTOM_LEFT_VERTEX] = Vector3i(x,y,z);	verts[CubeFace::BOTTOM_RIGHT_VERTEX] = Vector3i(x+1,y,z);
			break;
		case CubeFaceDesignator::FACE_XZ:
			verts[CubeFace::TOP_LEFT_VERTEX] = Vector3i(x,y,z+1);	verts[CubeFace::TOP_RIGHT_VERTEX] = Vector3i(x+1,y,z+1);
			verts[CubeFace::BOTTOM_LEFT_VERTEX] = Vector3i(x,y,z);	verts[CubeFace::BOTTOM_RIGHT_VERTEX] = Vector3i(x+1,y,z);
			break;
		case CubeFaceDesignator::FACE_YZ:
			verts[CubeFace::TOP_LEFT_VERTEX] = Vector3i(x,y,z+1);	verts[CubeFace::TOP_RIGHT_VERTEX] = Vector3i(x,y+1,z+1);
			verts[CubeFace::BOTTOM_LEFT_VERTEX] = Vector3i(x,y,z);	verts[CubeFace::BOTTOM_RIGHT_VERTEX] = Vector3i(x,y+1,z);
			break;
		default:
			throw Exception("Invalid face designator");		// this should never happen
	}
	return verts;
}

int CubeTable::lookup_cube_centre_node(int x, int y, int z) const
{
	std::map<Vector3i,int>::const_iterator it = m_cubeCentreNodes.find(Vector3i(x,y,z));
	if(it != m_cubeCentreNodes.end()) return it->second;
	else return -1;
}

boost::optional<const CubeFace&> CubeTable::lookup_cube_face(int x, int y, int z, CubeFaceDesignator::Enum f) const
{
	FaceSubtableCIter it = m_faceSubtables[f].find(Vector3i(x,y,z));
	if(it != m_faceSubtables[f].end()) return it->second;
	else return boost::none;
}

std::vector<boost::optional<const CubeFace&> > CubeTable::lookup_cube_faces(int x, int y, int z) const
{
	std::vector<boost::optional<const CubeFace&> > cubeFaces(6);
	cubeFaces[0] = lookup_cube_face(x,		y,		z,		CubeFaceDesignator::FACE_XY);
	cubeFaces[1] = lookup_cube_face(x,		y,		z+1,	CubeFaceDesignator::FACE_XY);
	cubeFaces[2] = lookup_cube_face(x,		y,		z,		CubeFaceDesignator::FACE_XZ);
	cubeFaces[3] = lookup_cube_face(x,		y+1,	z,		CubeFaceDesignator::FACE_XZ);
	cubeFaces[4] = lookup_cube_face(x,		y,		z,		CubeFaceDesignator::FACE_YZ);
	cubeFaces[5] = lookup_cube_face(x+1,	y,		z,		CubeFaceDesignator::FACE_YZ);
	return cubeFaces;
}

std::set<int> CubeTable::lookup_cube_nodes(int x, int y, int z) const
{
	std::set<int> nodeSet;

	std::vector<boost::optional<const CubeFace&> > cubeFaces = lookup_cube_faces(x,y,z);
	for(int i=0; i<6; ++i)
	{
		if(!cubeFaces[i]) continue;
		for(CubeFace::NodeDesignator n=enum_begin<CubeFace::NodeDesignator>(), end=enum_end<CubeFace::NodeDesignator>(); n!=end; ++n)
		{
			if(cubeFaces[i]->is_used(n)) nodeSet.insert(cubeFaces[i]->global_node_index(n));
		}
	}

	int cubeCentreNode = lookup_cube_centre_node(x,y,z);
	if(cubeCentreNode != -1) nodeSet.insert(cubeCentreNode);

	return nodeSet;
}

std::vector<int> CubeTable::lookup_face_centre_nodes(int x, int y, int z) const
{
	std::vector<int> faceCentreNodes;

	std::vector<boost::optional<const CubeFace&> > cubeFaces = lookup_cube_faces(x,y,z);
	for(int i=0; i<6; ++i)
	{
		if(!cubeFaces[i]) continue;
		if(cubeFaces[i]->is_used(CubeFace::MIDDLE_NODE))
		{
			faceCentreNodes.push_back(cubeFaces[i]->global_node_index(CubeFace::MIDDLE_NODE));
		}
	}

	return faceCentreNodes;
}

void CubeTable::set_cube_centre_node(int x, int y, int z, int cubeCentreNode)
{
	m_cubeCentreNodes.insert(std::make_pair(Vector3i(x,y,z), cubeCentreNode));
}

void CubeTable::set_cube_face(int x, int y, int z, CubeFaceDesignator::Enum f, const CubeFace& cubeFace)
{
	m_faceSubtables[f].insert(std::make_pair(Vector3i(x,y,z), cubeFace));
}

}
