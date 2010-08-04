/***
 * millipede: BaseCanvas.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_BASECANVAS
#define H_MILLIPEDE_BASECANVAS

#include <boost/shared_ptr.hpp>

#include <common/math/Vector2.h>
#include <common/math/Vector3.h>
#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/slices/SliceTextureSet.h>
#include <mast/gui/components/canvas/Canvas.h>
#include <mast/models/PartitionModel.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class DrawingTool> DrawingTool_Ptr;
typedef boost::shared_ptr<const class DrawingTool> DrawingTool_CPtr;
typedef boost::shared_ptr<class PartitionCamera> PartitionCamera_Ptr;
typedef boost::shared_ptr<const class PartitionCamera> PartitionCamera_CPtr;
typedef boost::shared_ptr<const class PartitionOverlayManager> PartitionOverlayManager_CPtr;
class PartitionView;
typedef boost::shared_ptr<const class Texture> Texture_CPtr;

class BaseCanvas : public Canvas
{
	//#################### TYPEDEFS ####################
protected:
	typedef DICOMImageBranchLayer BranchLayer;
	typedef AbdominalFeature::Enum Feature;
	typedef DICOMImageLeafLayer LeafLayer;
	typedef PartitionModel<LeafLayer,BranchLayer,Feature> PartitionModelT;
	typedef boost::shared_ptr<PartitionModelT> PartitionModel_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	PartitionView *m_partitionView;
	int m_wheelRotation;

	//#################### CONSTRUCTORS ####################
public:
	BaseCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);

	//#################### PRIVATE ABSTRACT METHODS ####################
private:
	virtual void render_overlays(double left, double top, double right, double bottom) const = 0;
	virtual Greyscale8SliceTextureSet_CPtr texture_set_to_display() const = 0;

	//#################### PUBLIC METHODS ####################
public:
	void fit_image_to_canvas();
	void render(wxPaintDC&) const;
	void setup(PartitionView *partitionView);
	void zoom_to_fit();

	//#################### PROTECTED METHODS ####################
protected:
	void calculate_image_bounds(Vector2i& tl_Pixels, Vector2i& br_Pixels) const;
	PartitionCamera_Ptr camera();
	PartitionCamera_CPtr camera() const;
	Vector2d centre_coords() const;
	Vector2d centre_pixels() const;
	Vector2i clamp_to_image_bounds(const Vector2i& p_Pixels) const;
	Vector2d coord_to_pixel_offset(const Vector2d& offset_Coords) const;
	Vector2d coords_to_pixels(const Vector2d& p_Coords) const;
	Vector2d coords_to_pixels(const Vector3d& p_Coords) const;
	DrawingTool_Ptr current_drawing_tool();
	DrawingTool_CPtr current_drawing_tool() const;
	Greyscale8SliceTextureSet_CPtr dicom_texture_set() const;
	PartitionModel_Ptr model();
	PartitionOverlayManager_CPtr overlay_manager() const;
	Greyscale8SliceTextureSet_CPtr partition_texture_set(int layer) const;
	Vector2d pixel_to_coord_offset(const Vector2d& offset_Pixels) const;
	Vector3d pixels_to_3d_coords(const Vector2d& p_Pixels) const;
	Vector2d pixels_to_coords(const Vector2d& p_Pixels) const;
	itk::Index<2> pixels_to_position(const Vector2d& p_Pixels) const;
	Vector2d project_to_2d(const Vector3d& p) const;
	itk::Index<3> project_to_3d(const itk::Index<2>& position) const;
	Vector3d project_to_3d(const Vector2d& p) const;
	bool within_image_bounds(const Vector2i& p_Pixels) const;

	//#################### PRIVATE METHODS ####################
private:
	void zoom_on(const Vector2d& zoomCentre_Pixels, int zoomLevelDelta);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ MOUSE ~~~~~~~~~~~~~~~~~~~~
	void OnEnterWindow(wxMouseEvent& e);
	void OnMouseWheel(wxMouseEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif
