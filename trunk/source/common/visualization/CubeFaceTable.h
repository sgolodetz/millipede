/***
 * millipede: CubeFaceTable.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBEFACETABLE
#define H_MILLIPEDE_CUBEFACETABLE

#include <map>

#include <boost/optional.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>

#include "CubeFace.h"
#include "CubeFaceDesignator.h"

namespace mp {

/**
@brief	A CubeFaceTable is a map from (x,y,z,faceDesignator) tuples to cube faces.

The reason it is implemented specially, rather than just using a std::map, is that there
are a small, finite number of possibilities for the cube face designator, so it is better
to store a 3D lookup table for each rather than a single large 4D lookup table.
*/
class CubeFaceTable
{
	//#################### TYPEDEFS ####################
private:
	typedef boost::tuples::tuple<int,int,int> IntTriple;
	typedef std::map<IntTriple,CubeFace> Subtable;
	typedef Subtable::const_iterator SubtableCIter;

	//#################### PRIVATE VARIABLES ####################
private:
	Subtable m_subtables[3];

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Looks up the cube face (if any) specified by (x,y,z,f).

	@param[in]	x	The x position of the cube in the volume
	@param[in]	y	The y position of the cube in the volume
	@param[in]	z	The z position of the cube in the volume
	@param[in]	f	The cube face designator
	@return	The corresponding cube face (if any), or boost::none otherwise
	*/
	boost::optional<const CubeFace&> lookup_cube_face(int x, int y, int z, CubeFaceDesignator::Enum f) const;

	/**
	@brief	Sets the cube face for (x,y,z,f).

	@param[in]	x			The x position of the cube in the volume
	@param[in]	y			The y position of the cube in the volume
	@param[in]	z			The z position of the cube in the volume
	@param[in]	f			The cube face designator
	@param[in]	cubeFace	The cube face itself
	@post
		-	*lookup_cube_face(x,y,z,f) refers to cubeFace
	*/
	void set_cube_face(int x, int y, int z, CubeFaceDesignator::Enum f, const CubeFace& cubeFace);
};

}

#endif
