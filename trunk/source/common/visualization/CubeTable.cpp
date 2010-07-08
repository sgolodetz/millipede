/***
 * millipede: CubeTable.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CubeTable.h"

namespace mp {

//#################### PUBLIC METHODS ####################
boost::optional<int> CubeTable::lookup_cube_centre_node(int x, int y, int z) const
{
	std::map<Vector3i,int>::const_iterator it = m_cubeCentreNodes.find(Vector3i(x,y,z));
	if(it != m_cubeCentreNodes.end()) return it->second;
	else return boost::none;
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

	boost::optional<int> cubeCentreNode = lookup_cube_centre_node(x,y,z);
	if(cubeCentreNode) nodeSet.insert(*cubeCentreNode);

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
