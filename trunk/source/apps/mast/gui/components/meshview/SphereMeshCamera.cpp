/***
 * millipede: SphereMeshCamera.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SphereMeshCamera.h"

#include <utility>

#include <common/ogl/WrappedGL.h>
#include <GL/glu.h>

namespace mp {

//#################### CONSTRUCTORS ####################
SphereMeshCamera::SphereMeshCamera(const Vector3i& centre, unsigned int distance,
								   const Vector3i& minCentre, const Vector3i& maxCentre,
								   unsigned int minDistance, unsigned int maxDistance)
:	m_azimuth(0), m_inclination(0)
{
	set_centre_range_impl(minCentre, maxCentre);
	set_centre(centre);
	set_distance_range_impl(minDistance, maxDistance);
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

const Vector3i& SphereMeshCamera::max_centre() const
{
	return m_maxCentre;
}

int SphereMeshCamera::max_distance() const
{
	return m_maxDistance;
}

const Vector3i& SphereMeshCamera::min_centre() const
{
	return m_minCentre;
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

void SphereMeshCamera::set_centre(const Vector3i& centre)
{
	set_centre_x(centre.x);
	set_centre_y(centre.y);
	set_centre_z(centre.z);
}

void SphereMeshCamera::set_centre_range(const Vector3i& minCentre, const Vector3i& maxCentre)
{
	set_centre_range_impl(minCentre, maxCentre);
	set_centre(m_centre);		// enforce the new centre constraints
}

void SphereMeshCamera::set_centre_x(int centreX)
{
	if(centreX < m_minCentre.x)			centreX = m_minCentre.x;
	else if(centreX > m_maxCentre.x)	centreX = m_maxCentre.x;
	m_centre.x = centreX;
}

void SphereMeshCamera::set_centre_y(int centreY)
{
	if(centreY < m_minCentre.y)			centreY = m_minCentre.y;
	else if(centreY > m_maxCentre.y)	centreY = m_maxCentre.y;
	m_centre.y = centreY;
}

void SphereMeshCamera::set_centre_z(int centreZ)
{
	if(centreZ < m_minCentre.z)			centreZ = m_minCentre.z;
	else if(centreZ > m_maxCentre.z)	centreZ = m_maxCentre.z;
	m_centre.z = centreZ;
}

void SphereMeshCamera::set_distance(unsigned int distance)
{
	if(distance < m_minDistance)		distance = m_minDistance;
	else if(distance > m_maxDistance)	distance = m_maxDistance;
	m_distance = distance;
}

void SphereMeshCamera::set_distance_range(unsigned int minDistance, unsigned int maxDistance)
{
	set_distance_range_impl(minDistance, maxDistance);
	set_distance(m_distance);		// enforce the new distance constraints
}

void SphereMeshCamera::set_inclination(int inclination)
{
	if(inclination < -89)		inclination = -89;
	else if(inclination > 89)	inclination = 89;
	m_inclination = inclination;
}

void SphereMeshCamera::use_as_view() const
{
	Vector3d at = Vector3d(m_centre);
	Vector3d up(0,0,-1);

	double A = m_azimuth * MathConstants::PI/180;
	double I = m_inclination * MathConstants::PI/180;

	// Calculate the eye position from the centre, azimuth, inclination and distance.
	double cosA = cos(A), sinA = sin(A), cosI = cos(I), sinI = sin(I);
	Vector3d eye(sinA*cosI, cosA*cosI, -sinI);
	eye *= m_distance;
	eye += at;

	gluLookAt(eye.x, eye.y, eye.z, at.x, at.y, at.z, up.x, up.y, up.z);
}

//#################### PRIVATE METHODS ####################
void SphereMeshCamera::set_centre_range_impl(const Vector3i& minCentre, const Vector3i& maxCentre)
{
	m_minCentre = minCentre;
	m_maxCentre = maxCentre;

	if(m_minCentre.x >= m_maxCentre.x) std::swap(m_minCentre.x, m_maxCentre.x);
	if(m_minCentre.y >= m_maxCentre.y) std::swap(m_minCentre.y, m_maxCentre.y);
	if(m_minCentre.z >= m_maxCentre.z) std::swap(m_minCentre.z, m_maxCentre.z);
}

void SphereMeshCamera::set_distance_range_impl(unsigned int minDistance, unsigned int maxDistance)
{
	if(minDistance > maxDistance) std::swap(minDistance, maxDistance);
	m_minDistance = minDistance;
	m_maxDistance = maxDistance;
}

}
