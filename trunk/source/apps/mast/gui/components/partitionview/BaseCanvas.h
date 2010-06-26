/***
 * millipede: BaseCanvas.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_BASECANVAS
#define H_MILLIPEDE_BASECANVAS

#include <boost/shared_ptr.hpp>

#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/CTImageBranchLayer.h>
#include <common/partitionforests/images/CTImageLeafLayer.h>
#include <mast/gui/components/canvas/Canvas.h>
#include "PartitionModel.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class PartitionCamera> PartitionCamera_Ptr;
typedef boost::shared_ptr<const class PartitionCamera> PartitionCamera_CPtr;
typedef boost::shared_ptr<const class PartitionOverlayManager> PartitionOverlayManager_CPtr;
class PartitionView;
typedef boost::shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;
typedef boost::shared_ptr<const class Texture> Texture_CPtr;

class BaseCanvas : public Canvas
{
	//#################### TYPEDEFS ####################
protected:
	typedef PartitionModel<CTImageLeafLayer,CTImageBranchLayer,AbdominalFeature> PartitionModelT;
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
	virtual SliceTextureSet_CPtr texture_set_to_display() const = 0;

	//#################### PUBLIC METHODS ####################
public:
	void fit_image_to_canvas();
	void render(wxPaintDC& dc) const;
	void setup(PartitionView *partitionView);
	void zoom_to_fit();

	//#################### PROTECTED METHODS ####################
protected:
	void calculate_image_bounds(itk::Vector<double,2>& tl_Pixels, itk::Vector<double,2>& br_Pixels) const;
	PartitionCamera_Ptr camera();
	PartitionCamera_CPtr camera() const;
	itk::Vector<double,2> centre_coords() const;
	itk::Vector<double,2> centre_pixels() const;
	itk::Vector<double,2> coord_to_pixel_offset(const itk::Vector<double,2>& offset_Coords) const;
	itk::Vector<double,2> coords_to_pixels(const itk::Vector<double,2>& p_Coords) const;
	itk::Vector<double,2> coords_to_pixels(const itk::Vector<double,3>& p_Coords) const;
	SliceTextureSet_CPtr dicom_texture_set() const;
	PartitionModel_Ptr model();
	PartitionOverlayManager_CPtr overlay_manager() const;
	SliceTextureSet_CPtr partition_texture_set(int layer) const;
	itk::Vector<double,2> pixel_to_coord_offset(const itk::Vector<double,2>& offset_Pixels) const;
	itk::Vector<double,3> pixels_to_3d_coords(const itk::Vector<double,2>& p_Pixels) const;
	itk::Vector<double,2> pixels_to_coords(const itk::Vector<double,2>& p_Pixels) const;
	itk::Vector<double,2> project_to_2d(const itk::Vector<double,3>& p) const;
	itk::Vector<double,3> project_to_3d(const itk::Vector<double,2>& p) const;

	//#################### PRIVATE METHODS ####################
private:
	void zoom_on(itk::Vector<double,2> zoomCentre_Pixels, int zoomLevelDelta);

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
