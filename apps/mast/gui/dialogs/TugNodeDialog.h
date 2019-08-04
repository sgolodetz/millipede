/***
 * millipede: TugNodeDialog.h
 * Copyright Stuart Golodetz, 2017. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TUGNODEDIALOG
#define H_MILLIPEDE_TUGNODEDIALOG

#include <boost/function.hpp>

#include <wx/dialog.h>

#include <millipede/partitionforests/base/PartitionForest.h>

//#################### FORWARD DECLARATIONS ####################
class wxRadioBox;
class wxSpinCtrl;

namespace mp {

class TugNodeDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	wxSpinCtrl *m_atomicLayer;
	boost::function<void(int)> m_setAtomicLayerHook;
	boost::function<void(int)> m_setToLayerOffsetHook;
	boost::function<void(TugMode)> m_setTugModeHook;
	wxSpinCtrl *m_toLayerOffset;
	wxRadioBox *m_tugMode;

	//#################### CONSTRUCTORS ####################
public:
	TugNodeDialog(wxWindow *parent, int nodeLayer, int atomicLayer, int toLayerOffset,
	              const boost::function<void(int)>& setAtomicLayerHook,
	              const boost::function<void(int)>& setToLayerOffsetHook,
	              const boost::function<void(TugMode)>& setTugModeHook);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonOK(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
	void OnRadioBoxTugMode(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ SPIN CONTROLS ~~~~~~~~~~~~~~~~~~~~
	void OnSpinAtomicLayer(wxSpinEvent&);
	void OnSpinToLayerOffset(wxSpinEvent&);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif
