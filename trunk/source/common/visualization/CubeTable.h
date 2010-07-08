/***
 * millipede: CubeTable.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBETABLE
#define H_MILLIPEDE_CUBETABLE

#include <map>
#include <set>
#include <vector>

#include <boost/optional.hpp>

#include <common/vectors/Vector3.h>
#include "CubeFace.h"
#include "CubeFaceDesignator.h"

namespace mp {

class CubeTable
{
	//#################### TYPEDEFS ####################
private:
	typedef std::map<Vector3i,CubeFace> FaceSubtable;
	typedef FaceSubtable::const_iterator FaceSubtableCIter;

	//#################### PRIVATE VARIABLES ####################
private:
	std::map<Vector3i,int> m_cubeCentreNodes;
	FaceSubtable m_faceSubtables[3];

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Looks up the node (if any) at the centre of cube (x,y,z).

	@param[in]	x	The x position of the cube in the volume
	@param[in]	y	The y position of the cube in the volume
	@param[in]	z	The z position of the cube in the volume
	@return	The index of the node (if any) in the global node table, or boost::none otherwise
	*/
	boost::optional<int> lookup_cube_centre_node(int x, int y, int z) const;

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
	@brief	Looks up all the cube faces (where present) for the cube (x,y,z).

	@param[in]	x	The x position of the cube in the volume
	@param[in]	y	The y position of the cube in the volume
	@param[in]	z	The z position of the cube in the volume
	@return	A std::vector containing the cube faces (where present)
	*/
	std::vector<boost::optional<const CubeFace&> > lookup_cube_faces(int x, int y, int z) const;

	/**
	@brief	Looks up all the nodes used by cube (x,y,z).

	@param[in]	x	The x position of the cube in the volume
	@param[in]	y	The y position of the cube in the volume
	@param[in]	z	The z position of the cube in the volume
	@return	A std::set containing the indices of the nodes used in the global node table
	*/
	std::set<int> lookup_cube_nodes(int x, int y, int z) const;

	/**
	@brief	Looks up all the face centre nodes in cube (x,y,z).

	@param[in]	x	The x position of the cube in the volume
	@param[in]	y	The y position of the cube in the volume
	@param[in]	z	The z position of the cube in the volume
	@return	A std::vector containing the indices of the face centre nodes in the global node table
	*/
	std::vector<int> lookup_face_centre_nodes(int x, int y, int z) const;

	/**
	@brief	Sets the node at the centre of cube (x,y,z).

	@param[in]	x				The x position of the cube in the volume
	@param[in]	y				The y position of the cube in the volume
	@param[in]	z				The z position of the cube in the volume
	@param[in]	cubeCentreNode	The index of the cube's centre node in the global node table
	*/
	void set_cube_centre_node(int x, int y, int z, int cubeCentreNode);

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
