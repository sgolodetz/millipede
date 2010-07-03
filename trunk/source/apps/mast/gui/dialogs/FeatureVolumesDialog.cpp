/***
 * millipede: FeatureVolumesDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "FeatureVolumesDialog.h"

namespace mp {

//#################### CONSTRUCTORS ####################
FeatureVolumesDialog::FeatureVolumesDialog(wxWindow *parent)
:	wxDialog(parent, wxID_ANY, wxT("Feature Volumes"), wxDefaultPosition, wxDefaultSize)
{
	CentreOnParent();
}

}
