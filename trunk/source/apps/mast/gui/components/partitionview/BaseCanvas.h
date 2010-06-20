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

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class PartitionCamera> PartitionCamera_CPtr;
typedef boost::shared_ptr<const class PartitionOverlayManager> PartitionOverlayManager_CPtr;
class PartitionView;
typedef boost::shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;
typedef boost::shared_ptr<const class Texture> Texture_CPtr;

class BaseCanvas : public Canvas
{
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
	void render(wxPaintDC& dc) const;
	void setup(PartitionView *partitionView);

	//#################### PROTECTED METHODS ####################
protected:
	PartitionCamera_CPtr camera() const;
	PartitionOverlayManager_CPtr overlay_manager() const;

	//#################### PRIVATE METHODS ####################
private:
	itk::Vector<double,2> coords_to_pixels(const itk::Vector<double,2>& p_Coords) const;
	itk::Vector<double,2> coords_to_pixels(const itk::Vector<double,3>& p_Coords) const;
	itk::Vector<double,2> project_to_2d(const itk::Vector<double,3>& p) const;

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
