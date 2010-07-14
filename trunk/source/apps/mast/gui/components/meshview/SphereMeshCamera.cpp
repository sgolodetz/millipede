/***
 * millipede: SphereMeshCamera.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SphereMeshCamera.h"

#include <utility>

namespace mp {

//#################### CONSTRUCTORS ####################
SphereMeshCamera::SphereMeshCamera(const Vector3i& centre, unsigned int distance, unsigned int minDistance, unsigned int maxDistance)
:	m_azimuth(0), m_centre(centre), m_inclination(0)
{
	set_distance_constraints(minDistance, maxDistance);
	set_distance(distance);
}

//#################### PUBLIC METHODS ####################
int SphereMeshCamera::azimuth() const
{
	return m_azimuth;
}

const Vector3i& SphereMeshCamera::centre() const
{
	return m_centre;
}

unsigned int SphereMeshCamera::distance() const
{
	return m_distance;
}

int SphereMeshCamera::inclination() const
{
	return m_inclination;
}

int SphereMeshCamera::max_distance() const
{
	return m_maxDistance;
}

int SphereMeshCamera::min_distance() const
{
	return m_minDistance;
}

void SphereMeshCamera::set_azimuth(int azimuth)
{
	if(azimuth < -179)		azimuth = -179;
	else if(azimuth > 180)	azimuth = 180;
	m_azimuth = azimuth;
}

void SphereMeshCamera::set_centre_x(int centreX)
{
	m_centre.x = centreX;
}

void SphereMeshCamera::set_centre_y(int centreY)
{
	m_centre.y = centreY;
}

void SphereMeshCamera::set_centre_z(int centreZ)
{
	m_centre.z = centreZ;
}

void SphereMeshCamera::set_distance(unsigned int distance)
{
	if(distance < m_minDistance)		distance = m_minDistance;
	else if(distance > m_maxDistance)	distance = m_maxDistance;
	m_distance = distance;
}

void SphereMeshCamera::set_distance_constraints(unsigned int minDistance, unsigned int maxDistance)
{
	if(minDistance > maxDistance) std::swap(minDistance, maxDistance);
	m_minDistance = minDistance;
	m_maxDistance = maxDistance;
	set_distance(m_distance);		// enforce the new distance constraints
}

void SphereMeshCamera::set_inclination(int inclination)
{
	if(inclination < -90)		inclination = -90;
	else if(inclination > 90)	inclination = 90;
	m_inclination = inclination;
}

void SphereMeshCamera::use_as_view() const
{
	// TODO
}

}
