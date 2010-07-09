/***
 * millipede: SchroederTriangulator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SCHROEDERTRIANGULATOR
#define H_MILLIPEDE_SCHROEDERTRIANGULATOR

#include <list>

#include "GlobalNodeTable.h"
#include "MeshTriangle.h"

namespace mp {

template <typename Label>
class SchroederTriangulator
{
	//#################### TYPEDEFS ####################
private:
	typedef GlobalNodeTable<Label> GlobalNodeTableT;

	//#################### PRIVATE VARIABLES ####################
private:
	const GlobalNodeTableT& m_globalNodeTable;

	//#################### CONSTRUCTORS ####################
public:
	explicit SchroederTriangulator(const GlobalNodeTableT& globalNodeTable)
	:	m_globalNodeTable(globalNodeTable)
	{}

	//#################### PUBLIC METHODS ####################
public:
	std::list<MeshTriangle> triangulate(const std::vector<int>& nodeLoop) const
	{
		std::list<MeshTriangle> triangles;

		// NYI
		return triangles;
	}
};

}

#endif
