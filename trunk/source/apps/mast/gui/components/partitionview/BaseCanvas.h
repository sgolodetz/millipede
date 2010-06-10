/***
 * millipede: BaseCanvas.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_BASECANVAS
#define H_MILLIPEDE_BASECANVAS

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/CTImageBranchLayer.h>
#include <common/partitionforests/images/CTImageLeafLayer.h>
#include <mast/gui/components/canvas/Canvas.h>
#include <mast/models/PartitionModel.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<const class PartitionOverlayManager> PartitionOverlayManager_CPtr;
class PartitionView;
typedef shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;
typedef shared_ptr<const class Texture> Texture_CPtr;

class BaseCanvas : public Canvas
{
	//#################### TYPEDEFS ####################
private:
	typedef shared_ptr<const PartitionModel<CTImageLeafLayer,CTImageBranchLayer,AbdominalFeature> > PartitionModel_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	const PartitionView *m_partitionView;

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
	void setup(const PartitionView *partitionView);

	//#################### PROTECTED METHODS ####################
protected:
	PartitionModel_CPtr model() const;
	PartitionOverlayManager_CPtr overlay_manager() const;
};

}

#endif
