/***
 * millipede: BaseCanvas.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_BASECANVAS
#define H_MILLIPEDE_BASECANVAS

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <mast/gui/base/Canvas.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class ViewedVolume> ViewedVolume_Ptr;

class BaseCanvas : public Canvas
{
	//#################### PRIVATE VARIABLES ####################
private:
	ViewedVolume_Ptr m_model;

	//#################### CONSTRUCTORS ####################
public:
	BaseCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);

	//#################### PUBLIC METHODS ####################
public:
	void render(wxPaintDC& dc) const;
	void setup(const ViewedVolume_Ptr& model);
};

}

#endif
