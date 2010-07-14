/***
 * millipede: SphereMeshCamera.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SPHEREMESHCAMERA
#define H_MILLIPEDE_SPHEREMESHCAMERA

#include <common/math/Vector3.h>
#include "MeshCamera.h"

namespace mp {

class SphereMeshCamera : public MeshCamera
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_azimuth;					///< the x-y plane angle in degrees, in the range (-180,180], where 0 is in the +y direction, -90 is in the +x direction, etc.
	Vector3i m_centre;				///< the point at which the camera is directed
	unsigned int m_distance;		///< the distance of the camera from the centre
	int m_inclination;				///< the inclination in degrees relative to the x-y plane, in the range [-90,90], where -90 is in the +z direction, etc.

	unsigned int m_minDistance;		///< the minimum distance of the camera from the centre
	unsigned int m_maxDistance;		///< the maximum distance of the camera from the centre

	//#################### CONSTRUCTORS ####################
public:
	SphereMeshCamera(const Vector3i& centre, unsigned int distance, unsigned int minDistance, unsigned int maxDistance);

	//#################### PUBLIC METHODS ####################
public:
	int azimuth() const;
	const Vector3i& centre() const;
	unsigned int distance() const;
	int inclination() const;
	int max_distance() const;
	int min_distance() const;
	void set_azimuth(int azimuth);
	void set_centre_x(int centreX);
	void set_centre_y(int centreY);
	void set_centre_z(int centreZ);
	void set_distance(unsigned int distance);
	void set_distance_constraints(unsigned int minDistance, unsigned int maxDistance);
	void set_inclination(int inclination);
	void use_as_view() const;
};

}

#endif
