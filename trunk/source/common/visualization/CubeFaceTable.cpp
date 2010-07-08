/***
 * millipede: CubeFaceTable.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CubeFaceTable.h"

namespace mp {

//#################### PUBLIC METHODS ####################
boost::optional<const CubeFace&> CubeFaceTable::lookup_cube_face(int x, int y, int z, CubeFaceDesignator::Enum f) const
{
	SubtableCIter it = m_subtables[f].find(Vector3i(x,y,z));
	if(it != m_subtables[f].end()) return it->second;
	else return boost::none;
}

void CubeFaceTable::set_cube_face(int x, int y, int z, CubeFaceDesignator::Enum f, const CubeFace& cubeFace)
{
	m_subtables[f].insert(std::make_pair(Vector3i(x,y,z), cubeFace));
}

}
