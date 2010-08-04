/***
 * millipede: BaseCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "BaseCanvas.h"

#include <common/dicom/volumes/DICOMVolume.h>
#include <common/exceptions/Exception.h>
#include <common/slices/SliceTextureSet.h>
#include <common/textures/Texture.h>
#include "PartitionCamera.h"
#include "PartitionView.h"

namespace mp {

//#################### CONSTRUCTORS ####################
BaseCanvas::BaseCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	Canvas(parent, context, attribList, id, pos, size, style), m_partitionView(NULL), m_wheelRotation(0)
{}

//#################### PUBLIC METHODS ####################
void BaseCanvas::fit_image_to_canvas()
{
	camera()->centre();
	zoom_to_fit();
}

void BaseCanvas::render(wxPaintDC&) const
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glTranslated(0, 0, -256);

	// Choose an image to render (if available).
	Texture_CPtr texture;
	Greyscale8SliceTextureSet_CPtr textureSet = texture_set_to_display();
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
		Vector2i tl_Pixels, br_Pixels;
		calculate_image_bounds(tl_Pixels, br_Pixels);

		// Render the image.
		glPushAttrib(GL_ENABLE_BIT);
		glEnable(GL_TEXTURE_2D);
		texture->bind();
		glColor3d(1,1,1);
		glBegin(GL_QUADS);
			glTexCoord2d(0,0);	glVertex2i(tl_Pixels.x, tl_Pixels.y);
			glTexCoord2d(1,0);	glVertex2i(br_Pixels.x + 1, tl_Pixels.y);
			glTexCoord2d(1,1);	glVertex2i(br_Pixels.x + 1, br_Pixels.y + 1);
			glTexCoord2d(0,1);	glVertex2i(tl_Pixels.x, br_Pixels.y + 1);
		glEnd();
		glPopAttrib();

		// Render any overlays for this canvas.
		render_overlays(tl_Pixels.x, tl_Pixels.y, br_Pixels.x + 1, br_Pixels.y + 1);
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

	// Set up the viewport.
	int width, height;
	GetSize(&width, &height);
	glViewport(0, 0, width, height);

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

	// Set up the clear colour.
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

	// Set up the projection matrix.
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, width, height, 0, 0.0, 2048.0);
}

void BaseCanvas::zoom_to_fit()
{
	// Step 1:	Calculate the sizes of the image and canvas.
	Vector2i tl_Pixels, br_Pixels;
	calculate_image_bounds(tl_Pixels, br_Pixels);
	double imageWidth = br_Pixels.x + 1 - tl_Pixels.x, imageHeight = br_Pixels.y + 1 - tl_Pixels.y;
	wxSize canvasSize = GetSize();
	double canvasWidth = canvasSize.GetWidth(), canvasHeight = canvasSize.GetHeight();

	// Step 2:	Is either image dimension too big? If so, try and zoom out as far as necessary, and then return.
	if(imageWidth > canvasWidth || imageHeight > canvasHeight)
	{
		int newZoomLevel = camera()->zoom_level();
		while(newZoomLevel != camera()->min_zoom_level())
		{
			--newZoomLevel;
			double zoomFactor = camera()->zoom_factor(newZoomLevel) / camera()->zoom_factor();
			double newImageWidth = imageWidth * zoomFactor;
			double newImageHeight = imageHeight * zoomFactor;
			if(newImageWidth <= canvasWidth && newImageHeight <= canvasHeight)
			{
				break;
			}
		}
		camera()->set_zoom_level(newZoomLevel);
		return;
	}

	// Step 3:	Are both image dimensions too small? If so, try and zoom in as much as possible, and then return.
	if(imageWidth < canvasWidth && imageHeight < canvasHeight)
	{
		int newZoomLevel = camera()->zoom_level();
		while(newZoomLevel != camera()->max_zoom_level())
		{
			++newZoomLevel;
			double zoomFactor = camera()->zoom_factor(newZoomLevel) / camera()->zoom_factor();
			double newImageWidth = imageWidth * zoomFactor;
			double newImageHeight = imageHeight * zoomFactor;
			if(newImageWidth > canvasWidth || newImageHeight > canvasHeight)
			{
				// Gone one level too far.
				--newZoomLevel;
				break;
			}
		}
		camera()->set_zoom_level(newZoomLevel);
	}
}

//#################### PROTECTED METHODS ####################
void BaseCanvas::calculate_image_bounds(Vector2i& tl_Pixels, Vector2i& br_Pixels) const
{
	itk::Size<3> volumeSize = m_partitionView->model()->dicom_volume()->size();
	Vector3d tl_Coords(0,0,0);
	Vector3d br_Coords(volumeSize[0] - 1, volumeSize[1] - 1, volumeSize[2] - 1);
	tl_Pixels = Vector2i(coords_to_pixels(tl_Coords));
	br_Pixels = Vector2i(coords_to_pixels(br_Coords));
}

PartitionCamera_Ptr BaseCanvas::camera()
{
	if(m_partitionView) return m_partitionView->camera();
	else return PartitionCamera_Ptr();
}

PartitionCamera_CPtr BaseCanvas::camera() const
{
	if(m_partitionView) return m_partitionView->camera();
	else return PartitionCamera_CPtr();
}

Vector2d BaseCanvas::centre_coords() const
{
	SliceLocation loc = camera()->slice_location();
	return project_to_2d(Vector3d(loc.x, loc.y, loc.z));
}

Vector2d BaseCanvas::centre_pixels() const
{
	int width, height;
	GetSize(&width, &height);
	return Vector2d(width * 0.5, height * 0.5);
}

Vector2i BaseCanvas::clamp_to_image_bounds(const Vector2i& p_Pixels) const
{
	Vector2i tl_Pixels, br_Pixels;
	calculate_image_bounds(tl_Pixels, br_Pixels);
	return Vector2i(std::min(std::max(p_Pixels.x, tl_Pixels.x), br_Pixels.x), std::min(std::max(p_Pixels.y, tl_Pixels.y), br_Pixels.y));
}

Vector2d BaseCanvas::coord_to_pixel_offset(const Vector2d& offset_Coords) const
{
	// Calculate the scale factors in each of the dimensions and project into 2D to get the 2D factors.
	double zoomFactor = camera()->zoom_factor();
	Vector3d spacing = m_partitionView->model()->dicom_volume()->spacing();
	Vector2d scaleFactors = project_to_2d(zoomFactor * spacing);

	// Scale to get the offset in Pixels.
	Vector2d offset_Pixels = offset_Coords * scaleFactors;
	return offset_Pixels;
}

Vector2d BaseCanvas::coords_to_pixels(const Vector2d& p_Coords) const
{
	Vector2d centre_Coords = centre_coords();
	Vector2d offset_Coords = p_Coords - centre_Coords;
	Vector2d offset_Pixels = coord_to_pixel_offset(offset_Coords);
	Vector2d centre_Pixels = centre_pixels();
	return centre_Pixels + offset_Pixels;
}

Vector2d BaseCanvas::coords_to_pixels(const Vector3d& p_Coords) const
{
	return coords_to_pixels(project_to_2d(p_Coords));
}

DrawingTool_Ptr BaseCanvas::current_drawing_tool()
{
	if(m_partitionView) return m_partitionView->current_drawing_tool();
	else return DrawingTool_Ptr();
}

DrawingTool_CPtr BaseCanvas::current_drawing_tool() const
{
	if(m_partitionView) return m_partitionView->current_drawing_tool();
	else return DrawingTool_CPtr();
}

Greyscale8SliceTextureSet_CPtr BaseCanvas::dicom_texture_set() const
{
	if(m_partitionView) return m_partitionView->dicom_texture_set();
	else return Greyscale8SliceTextureSet_CPtr();
}

BaseCanvas::PartitionModel_Ptr BaseCanvas::model()
{
	if(m_partitionView) return m_partitionView->model();
	else return PartitionModel_Ptr();
}

PartitionOverlayManager_CPtr BaseCanvas::overlay_manager() const
{
	if(m_partitionView) return m_partitionView->overlay_manager();
	else return PartitionOverlayManager_CPtr();
}

Greyscale8SliceTextureSet_CPtr BaseCanvas::partition_texture_set(int layer) const
{
	if(m_partitionView) return m_partitionView->partition_texture_set(layer);
	else return Greyscale8SliceTextureSet_CPtr();
}

Vector2d BaseCanvas::pixel_to_coord_offset(const Vector2d& offset_Pixels) const
{
	// Calculate the scale factors in each of the dimensions and project into 2D to get the 2D factors.
	double zoomFactor = camera()->zoom_factor();
	Vector3d spacing = m_partitionView->model()->dicom_volume()->spacing();
	Vector2d scaleFactors = project_to_2d(zoomFactor * spacing);

	// Scale to get the offset in Coords.
	Vector2d offset_Coords = offset_Pixels / scaleFactors;
	return offset_Coords;
}

Vector3d BaseCanvas::pixels_to_3d_coords(const Vector2d& p_Pixels) const
{
	return project_to_3d(pixels_to_coords(p_Pixels));
}

Vector2d BaseCanvas::pixels_to_coords(const Vector2d& p_Pixels) const
{
	Vector2d centre_Pixels = centre_pixels();
	Vector2d offset_Pixels = p_Pixels - centre_Pixels;
	Vector2d offset_Coords = pixel_to_coord_offset(offset_Pixels);
	Vector2d centre_Coords = centre_coords();
	return centre_Coords + offset_Coords;
}

itk::Index<2> BaseCanvas::pixels_to_position(const Vector2d& p_Pixels) const
{
	Vector2d p_Coords = pixels_to_coords(p_Pixels);
	itk::Index<2> position;
	for(int i=0; i<2; ++i)
	{
		position[i] = NumericUtil::round_to_nearest<long>(p_Coords[i]);
	}
	return position;
}

Vector2d BaseCanvas::project_to_2d(const Vector3d& p) const
{
	switch(camera()->slice_orientation())
	{
		case ORIENT_XY:	return Vector2d(p.x, p.y);
		case ORIENT_XZ:	return Vector2d(p.x, p.z);
		case ORIENT_YZ:	return Vector2d(p.y, p.z);
	}

	// This should never happen.
	throw Exception("Bad slice orientation");
}

itk::Index<3> BaseCanvas::project_to_3d(const itk::Index<2>& position) const
{
	switch(camera()->slice_orientation())
	{
		case ORIENT_XY:	return ITKImageUtil::make_index(position[0], position[1], camera()->slice_location().z);
		case ORIENT_XZ:	return ITKImageUtil::make_index(position[0], camera()->slice_location().y, position[1]);
		case ORIENT_YZ:	return ITKImageUtil::make_index(camera()->slice_location().x, position[0], position[1]);
	}

	// This should never happen.
	throw Exception("Bad slice orientation");
}

Vector3d BaseCanvas::project_to_3d(const Vector2d& p) const
{
	switch(camera()->slice_orientation())
	{
		case ORIENT_XY:	return Vector3d(p.x, p.y, camera()->slice_location().z);
		case ORIENT_XZ:	return Vector3d(p.x, camera()->slice_location().y, p.y);
		case ORIENT_YZ:	return Vector3d(camera()->slice_location().x, p.x, p.y);
	}

	// This should never happen.
	throw Exception("Bad slice orientation");
}

bool BaseCanvas::within_image_bounds(const Vector2i& p_Pixels) const
{
	Vector2i tl_Pixels, br_Pixels;
	calculate_image_bounds(tl_Pixels, br_Pixels);
	for(int i=0; i<2; ++i)
	{
		if(p_Pixels[i] < tl_Pixels[i] || p_Pixels[i] > br_Pixels[i])
		{
			return false;
		}
	}
	return true;
}

//#################### PRIVATE METHODS ####################
void BaseCanvas::zoom_on(const Vector2d& zoomCentre_Pixels, int zoomLevelDelta)
{
	// Calculate the offset of the zoom centre from the centre in Pixels.
	Vector2d zoomCentreOffset_Pixels = zoomCentre_Pixels - centre_pixels();

	// Calculate the new centre in Pixels.
	int newZoomLevel = camera()->zoom_level() + zoomLevelDelta;
	double zoomFactor = camera()->zoom_factor(newZoomLevel) / camera()->zoom_factor();
	Vector2d newCentre_Pixels = zoomCentre_Pixels - zoomCentreOffset_Pixels / zoomFactor;

	// Clamp it to the image bounds.
	newCentre_Pixels = Vector2d(clamp_to_image_bounds(Vector2i(newCentre_Pixels)));

	// Calculate the new centre in Coords.
	Vector2d newCentre_Coords = pixels_to_coords(newCentre_Pixels);

	// Set the zoom level.
	if(!camera()->set_zoom_level(newZoomLevel)) return;

	// Set the slice location.
	SliceLocation loc = camera()->slice_location();
	switch(camera()->slice_orientation())
	{
		case ORIENT_XY:
			loc.x = static_cast<int>(newCentre_Coords.x);
			loc.y = static_cast<int>(newCentre_Coords.y);
			break;
		case ORIENT_XZ:
			loc.x = static_cast<int>(newCentre_Coords.x);
			loc.z = static_cast<int>(newCentre_Coords.y);
			break;
		case ORIENT_YZ:
			loc.y = static_cast<int>(newCentre_Coords.x);
			loc.z = static_cast<int>(newCentre_Coords.y);
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
		Vector2d zoomCentre(e.GetX(), e.GetY());
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
