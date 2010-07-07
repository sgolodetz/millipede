/***
 * millipede: CubeFace.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBEFACE
#define H_MILLIPEDE_CUBEFACE

namespace mp {

/**
@brief	A CubeFace maintains a map from local face node IDs to global node IDs.

There are five possible local positions for a node on the cube face, corresponding
to the midpoints of the four edges and the face centre. Not every node will be used
on each cube face.
*/
class CubeFace
{
	//#################### CONSTANTS ####################
public:
	/**
	@brief	An enum representing the possible face node locations.
	*/
	enum FaceNodeDesignator
	{
		TOP,
		LEFT,
		MIDDLE,
		RIGHT,
		BOTTOM,
		POTENTIAL_NODE_COUNT,	// = 5, since there are five potential nodes on a face: top, left, middle, right and bottom (numbered here in that order)
	};
private:
	static const int UNUSED = -1;
	static const int USED = -2;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_localToGlobalNodeMap[POTENTIAL_NODE_COUNT];

	//#################### CONSTRUCTORS ####################
public:
	/**
	@brief	Constructs a cube face in which all the local nodes are initially unused.
	*/
	CubeFace();

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Returns the global node ID of the specified local node.

	@param[in]	n	The face node designator of the local node
	@return	The global node ID of the local node, if any, or an unspecified value < 0 otherwise
	*/
	int global_node_id(FaceNodeDesignator n) const;

	/**
	@brief	Returns whether or not the specified local node is in use.

	@param[in]	n	The face node designator of the local node
	@return	true, if it is in use, or false otherwise
	*/
	bool is_used(FaceNodeDesignator n) const;

	/**
	@brief	Sets the global node ID of the specified local node.

	@param[in]	n	The face node designator of the local node
	@param[in]	id	The global node ID to assign to it
	@post
		-	global_node_id(n) == id
	*/
	void set_global_node_id(FaceNodeDesignator n, int id);

	/**
	@brief	Marks the specified local node as used, without setting its global node ID yet.

	@param[in]	n	The face designator of the local node
	@post
		-	is_used(n) == true
	*/
	void set_used(FaceNodeDesignator n);
};

}

#endif
