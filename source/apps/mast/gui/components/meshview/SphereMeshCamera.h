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
	int m_inclination;				///< the inclination in degrees relative to the x-y plane, in the range (-90,90), where -90 is in the +z direction, etc.

	Vector3i m_minCentre;			///< the lower bounds for the centre position
	Vector3i m_maxCentre;			///< the upper bounds for the centre position
	unsigned int m_minDistance;		///< the minimum distance of the camera from the centre
	unsigned int m_maxDistance;		///< the maximum distance of the camera from the centre

	//#################### CONSTRUCTORS ####################
public:
	SphereMeshCamera(const Vector3i& centre, unsigned int distance, int azimuth, int inclination, const Vector3i& minCentre, const Vector3i& maxCentre, unsigned int minDistance, unsigned int maxDistance);

	//#################### PUBLIC METHODS ####################
public:
	int azimuth() const;
	const Vector3i& centre() const;
	unsigned int distance() const;
	int inclination() const;
	const Vector3i& max_centre() const;
	int max_distance() const;
	const Vector3i& min_centre() const;
	int min_distance() const;
	void set_azimuth(int azimuth);
	void set_centre(const Vector3i& centre);
	void set_centre_range(const Vector3i& minCentre, const Vector3i& maxCentre);
	void set_distance(unsigned int distance);
	void set_distance_range(unsigned int minDistance, unsigned int maxDistance);
	void set_inclination(int inclination);
	void use_as_view() const;

	//#################### PRIVATE METHODS ####################
private:
	void set_centre_range_impl(const Vector3i& minCentre, const Vector3i& maxCentre);
	void set_distance_range_impl(unsigned int minDistance, unsigned int maxDistance);
};

}

#endif
