/***
 * millipede: BaseCanvas.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_BASECANVAS
#define H_MILLIPEDE_BASECANVAS

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <mast/gui/util/Canvas.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class PartitionModel> PartitionModel_Ptr;
typedef shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;
typedef shared_ptr<const class Texture> Texture_CPtr;

class BaseCanvas : public Canvas
{
	//#################### PROTECTED VARIABLES ####################
protected:
	PartitionModel_Ptr m_model;

	//#################### CONSTRUCTORS ####################
public:
	BaseCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);

	//#################### PRIVATE ABSTRACT METHODS ####################
private:
	virtual SliceTextureSet_CPtr texture_set_to_display() const = 0;

	//#################### PUBLIC METHODS ####################
public:
	void render(wxPaintDC& dc) const;
	void setup(const PartitionModel_Ptr& model);
};

}

#endif
