/***
 * millipede: PartitionOverlay.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "PartitionOverlay.h"

#include <common/textures/Texture.h>

namespace mp {

//#################### DESTRUCTOR ####################
PartitionOverlay::~PartitionOverlay()
{}

//#################### PUBLIC METHODS ####################
bool PartitionOverlay::on_dicom_canvas() const
{
	return true;
}

bool PartitionOverlay::on_partition_canvas() const
{
	return true;
}

void PartitionOverlay::render(double left, double top, double right, double bottom) const
{
	glPushAttrib(GL_ENABLE_BIT);

	glEnable(GL_TEXTURE_2D);
	m_texture->bind();
	glBegin(GL_QUADS);
		glTexCoord2d(0,0);	glVertex2d(left, top);
		glTexCoord2d(1,0);	glVertex2d(right, top);
		glTexCoord2d(1,1);	glVertex2d(right, bottom);
		glTexCoord2d(0,1);	glVertex2d(left, bottom);
	glEnd();

	glPopAttrib();
}

//#################### PROTECTED METHODS ####################
void PartitionOverlay::set_texture(const Texture_Ptr& texture)
{
	m_texture = texture;
}

}
