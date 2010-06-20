/***
 * millipede: BaseCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "BaseCanvas.h"

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/exceptions/Exception.h>
#include <common/slices/SliceTextureSet.h>
#include <common/textures/Texture.h>
#include "PartitionView.h"

namespace mp {

//#################### CONSTRUCTORS ####################
BaseCanvas::BaseCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	Canvas(parent, context, attribList, id, pos, size, style), m_wheelRotation(0)
{}

//#################### PUBLIC METHODS ####################
void BaseCanvas::render(wxPaintDC& dc) const
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glTranslated(0, 0, -256);

	// Choose an image to render (if available).
	Texture_CPtr texture;
	SliceTextureSet_CPtr textureSet = texture_set_to_display();
	if(textureSet)
	{
		assert(camera() != NULL);	// the texture set will have come from the camera, so the camera should be non-null
		switch(camera()->slice_orientation())
		{
			case ORIENT_XY:		texture = textureSet->texture(ORIENT_XY, camera()->slice_location().z); break;
			case ORIENT_XZ:		texture = textureSet->texture(ORIENT_XZ, camera()->slice_location().y); break;
			case ORIENT_YZ:		texture = textureSet->texture(ORIENT_YZ, camera()->slice_location().x); break;
			default:			throw Exception("Unexpected slice orientation");
		}
	}

	if(texture)
	{
		itk::Size<3> volumeSize = m_partitionView->model()->dicom_volume()->size();
		itk::Vector<double,3> tl_Coords;
		tl_Coords.Fill(0);
		itk::Vector<double,3> br_Coords;
		for(int i=0; i<3; ++i) br_Coords[i] = volumeSize[i];
		itk::Vector<double,2> tl_Pixels = coords_to_pixels(tl_Coords);
		itk::Vector<double,2> br_Pixels = coords_to_pixels(br_Coords);

		// Render the image.
		glPushAttrib(GL_ENABLE_BIT);
		glEnable(GL_TEXTURE_2D);
		texture->bind();
		glColor3d(1,1,1);
		glBegin(GL_QUADS);
			glTexCoord2d(0,0);	glVertex2d(tl_Pixels[0], tl_Pixels[1]);
			glTexCoord2d(1,0);	glVertex2d(br_Pixels[0], tl_Pixels[1]);
			glTexCoord2d(1,1);	glVertex2d(br_Pixels[0], br_Pixels[1]);
			glTexCoord2d(0,1);	glVertex2d(tl_Pixels[0], br_Pixels[1]);
		glEnd();
		glPopAttrib();

		// Render any overlays for this canvas.
		render_overlays(tl_Pixels[0], tl_Pixels[1], br_Pixels[0], br_Pixels[1]);
	}
	else
	{
		// Draw a cross to indicate that it's deliberate that no image is being displayed.
		int width, height;
		GetSize(&width, &height);
		glColor3d(1,1,1);
		glBegin(GL_LINES);
			glVertex2i(0,0);
			glVertex2i(width,height);
			glVertex2i(width,0);
			glVertex2i(0,height);
		glEnd();
	}
}

void BaseCanvas::setup(PartitionView *partitionView)
{
	m_partitionView = partitionView;

	SetCurrent();

	int width, height;
	GetSize(&width, &height);

	// Enable back-face culling.
	glCullFace(GL_BACK);
	glFrontFace(GL_CW);
	glEnable(GL_CULL_FACE);

	// Set up the z-buffer.
	glDepthFunc(GL_LEQUAL);
	glEnable(GL_DEPTH_TEST);

	// Set up alpha testing.
	glAlphaFunc(GL_NOTEQUAL, 0);
	glEnable(GL_ALPHA_TEST);

	glClearColor(0, 0, 0, 0);

	glViewport(0, 0, width, height);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();

	glOrtho(0, width, height, 0, 0.0, 2048.0);
}

//#################### PROTECTED METHODS ####################
PartitionCamera_CPtr BaseCanvas::camera() const
{
	return m_partitionView ? m_partitionView->camera() : PartitionCamera_CPtr();
}

PartitionOverlayManager_CPtr BaseCanvas::overlay_manager() const
{
	return m_partitionView ? m_partitionView->overlay_manager() : PartitionOverlayManager_CPtr();
}

//#################### PRIVATE METHODS ####################
const PartitionCamera_Ptr& BaseCanvas::camera()
{
	return m_partitionView->camera();
}

itk::Vector<double,2> BaseCanvas::centre_coords() const
{
	SliceLocation loc = camera()->slice_location();
	itk::Vector<double,3> centre3D_Coords;
	centre3D_Coords[0] = loc.x, centre3D_Coords[1] = loc.y, centre3D_Coords[2] = loc.z;
	return project_to_2d(centre3D_Coords);
}

itk::Vector<double,2> BaseCanvas::centre_pixels() const
{
	int width, height;
	GetSize(&width, &height);
	itk::Vector<double,2> centre_Pixels;
	centre_Pixels[0] = width * 0.5, centre_Pixels[1] = height * 0.5;
	return centre_Pixels;
}

itk::Vector<double,2> BaseCanvas::coord_to_pixel_offset(const itk::Vector<double,2>& offset_Coords) const
{
	// Calculate the scale factors in each of the dimensions and project into 2D to get the 2D factors.
	double zoomFactor = camera()->zoom_factor();
	itk::Vector<double,3> spacing = m_partitionView->model()->dicom_volume()->spacing();
	itk::Vector<double,2> scaleFactors = project_to_2d(zoomFactor * spacing);

	// Scale to get the offset in Pixels.
	itk::Vector<double,2> offset_Pixels;
	for(int i=0; i<2; ++i) offset_Pixels[i] = offset_Coords[i] * scaleFactors[i];

	return offset_Pixels;
}

itk::Vector<double,2> BaseCanvas::coords_to_pixels(const itk::Vector<double,2>& p_Coords) const
{
	itk::Vector<double,2> centre_Coords = centre_coords();
	itk::Vector<double,2> offset_Coords = p_Coords - centre_Coords;
	itk::Vector<double,2> offset_Pixels = coord_to_pixel_offset(offset_Coords);
	itk::Vector<double,2> centre_Pixels = centre_pixels();
	return centre_Pixels + offset_Pixels;
}

itk::Vector<double,2> BaseCanvas::coords_to_pixels(const itk::Vector<double,3>& p_Coords) const
{
	return coords_to_pixels(project_to_2d(p_Coords));
}

itk::Vector<double,2> BaseCanvas::pixel_to_coord_offset(const itk::Vector<double,2>& offset_Pixels) const
{
	// Calculate the scale factors in each of the dimensions and project into 2D to get the 2D factors.
	double zoomFactor = camera()->zoom_factor();
	itk::Vector<double,3> spacing = m_partitionView->model()->dicom_volume()->spacing();
	itk::Vector<double,2> scaleFactors = project_to_2d(zoomFactor * spacing);

	// Scale to get the offset in Coords.
	itk::Vector<double,2> offset_Coords;
	for(int i=0; i<2; ++i) offset_Coords[i] = offset_Pixels[i] / scaleFactors[i];

	return offset_Coords;
}

itk::Vector<double,2> BaseCanvas::pixels_to_coords(const itk::Vector<double,2>& p_Pixels) const
{
	itk::Vector<double,2> centre_Pixels = centre_pixels();
	itk::Vector<double,2> offset_Pixels = p_Pixels - centre_Pixels;
	itk::Vector<double,2> offset_Coords = pixel_to_coord_offset(offset_Pixels);
	itk::Vector<double,2> centre_Coords = centre_coords();
	return centre_Coords + offset_Coords;
}

itk::Vector<double,2> BaseCanvas::project_to_2d(const itk::Vector<double,3>& p) const
{
	itk::Vector<double,2> ret;
	switch(camera()->slice_orientation())
	{
		case ORIENT_XY:
			ret[0] = p[0];
			ret[1] = p[1];
			break;
		case ORIENT_XZ:
			ret[0] = p[0];
			ret[1] = p[2];
			break;
		case ORIENT_YZ:
			ret[0] = p[1];
			ret[1] = p[2];
			break;
	}
	return ret;
}

void BaseCanvas::zoom_on(itk::Vector<double,2> zoomCentre_Pixels, int zoomLevelDelta)
{
	// Calculate the offset of the zoom centre from the centre in Pixels.
	itk::Vector<double,2> zoomCentreOffset_Pixels = zoomCentre_Pixels - centre_pixels();

	// Calculate the new centre in Pixels.
	int newZoomLevel = camera()->zoom_level() + zoomLevelDelta;
	double zoomFactor = camera()->zoom_factor(newZoomLevel) / camera()->zoom_factor();
	itk::Vector<double,2> newCentre_Pixels = zoomCentre_Pixels - zoomCentreOffset_Pixels / zoomFactor;

	// Calculate the new centre in Coords.
	itk::Vector<double,2> newCentre_Coords = pixels_to_coords(newCentre_Pixels);

	// Clamp it to the volume.
	itk::Size<3> volumeSize = m_partitionView->model()->dicom_volume()->size();
	itk::Vector<double,3> maxs3D;
	for(int i=0; i<3; ++i)
	{
		maxs3D[i] = volumeSize[i] - 1;
	}
	itk::Vector<double,2> maxs = project_to_2d(maxs3D);
	for(int i=0; i<2; ++i)
	{
		if(newCentre_Coords[i] < 0) newCentre_Coords[i] = 0;
		if(newCentre_Coords[i] > maxs[i]) newCentre_Coords[i] = maxs[i];
	}

	// Set the zoom level.
	if(!camera()->set_zoom_level(newZoomLevel)) return;

	// Set the slice location.
	SliceLocation loc = camera()->slice_location();
	switch(camera()->slice_orientation())
	{
		case ORIENT_XY:
			loc.x = static_cast<int>(newCentre_Coords[0]);
			loc.y = static_cast<int>(newCentre_Coords[1]);
			break;
		case ORIENT_XZ:
			loc.x = static_cast<int>(newCentre_Coords[0]);
			loc.z = static_cast<int>(newCentre_Coords[1]);
			break;
		case ORIENT_YZ:
			loc.y = static_cast<int>(newCentre_Coords[0]);
			loc.z = static_cast<int>(newCentre_Coords[1]);
			break;
	}
	camera()->set_slice_location(loc);
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ MOUSE ~~~~~~~~~~~~~~~~~~~~
void BaseCanvas::OnEnterWindow(wxMouseEvent& e)
{
	SetFocus();
}

void BaseCanvas::OnMouseWheel(wxMouseEvent& e)
{
	m_wheelRotation += e.GetWheelRotation();
	int lines = m_wheelRotation / e.GetWheelDelta();
	m_wheelRotation -= lines * e.GetWheelDelta();

	if(lines != 0)
	{
		itk::Vector<double,2> zoomCentre;
		zoomCentre[0] = e.GetX();
		zoomCentre[1] = e.GetY();
		int zoomLevelDelta = lines * 1;		// I've left it like this to allow the amount to zoom to be easily changed in future
		zoom_on(zoomCentre, zoomLevelDelta);
	}
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(BaseCanvas, Canvas)
	//~~~~~~~~~~~~~~~~~~~~ MOUSE ~~~~~~~~~~~~~~~~~~~~
	EVT_ENTER_WINDOW(BaseCanvas::OnEnterWindow)
	EVT_MOUSEWHEEL(BaseCanvas::OnMouseWheel)
END_EVENT_TABLE()

}
