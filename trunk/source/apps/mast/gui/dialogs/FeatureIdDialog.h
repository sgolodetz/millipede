/***
 * millipede: SegmentDICOMVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FEATUREIDDIALOG
#define H_MILLIPEDE_FEATUREIDDIALOG

#include <wx/textctrl.h>

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <boost/optional.hpp>

#include <vector>

#include <common/segmentation/DICOMSegmentationOptions.h>
#include <mast/models/SegmentationRun.h>
#include <common/jobs/CompositeJob.h>
#include <string>
#include <common/featureid/FIOptions.h>


class wxSpinCtrl;

namespace mp {
	
namespace mp_FeatureIdDialog {	
	
//#################### ENUMERATIONS ####################
enum
{
	BUTTONID_OK,
	BUTTONID_CANCEL,
	BUTTONID_LOAD,
	BUTTONID_SAVE
};

class FeatureIdDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:

	// list of parameters for perl generation of repetetive definitions etc.
	// @l = qw(KMaxVox KMinVox KMaxH KMinH KMaxAspect KMinAspect KSeedTolerance KAdjTolerance KSpineClose KRibDist AMaxVox AMinVox AMaxH AMinH LMinVox LRibDist LSeedTolerance LAdjTolerance LMinH LMaxH LMorphMinH RMinSeedH RMaxVox RMinGrowH RBoundaryScale ROutsideMaxH RPPMinH RPPMinVox RPPMaxVox DoSpine DoSpinal DoRibs DoAorta DoLiver DoKidneys DoSpleen)

	
	FIOptions options;
	
	wxWindow* m_parent;
	wxSpinCtrl * m_KMaxVoxSpin;
	wxSpinCtrl * m_KMinVoxSpin;
	wxSpinCtrl * m_KMaxHSpin;
	wxSpinCtrl * m_KMinHSpin;
	wxSpinCtrl * m_KMaxAspectSpin;
	wxSpinCtrl * m_KMinAspectSpin;
	wxSpinCtrl * m_KSeedToleranceSpin;
	wxSpinCtrl * m_KAdjToleranceSpin;
	wxSpinCtrl * m_LSeedToleranceSpin;
	wxSpinCtrl * m_LAdjToleranceSpin;
	wxSpinCtrl * m_KSpineCloseSpin;
	wxSpinCtrl * m_KRibDistSpin;
	wxSpinCtrl * m_AMaxVoxSpin;
	wxSpinCtrl * m_AMinVoxSpin;
	wxSpinCtrl * m_AMaxHSpin;
	wxSpinCtrl * m_AMinHSpin;
	wxSpinCtrl * m_LMinVoxSpin;
	wxSpinCtrl * m_LRibDistSpin;
	wxSpinCtrl * m_LMinHSpin;
	wxSpinCtrl * m_LMaxHSpin;
	wxSpinCtrl * m_LMorphMinHSpin;
	wxSpinCtrl * m_RMinSeedHSpin;
	wxSpinCtrl * m_RMaxVoxSpin;
	wxSpinCtrl * m_RMinGrowHSpin;
	wxSpinCtrl * m_RBoundaryScaleSpin;
	wxSpinCtrl * m_ROutsideMaxHSpin;
	wxSpinCtrl * m_RPPMinHSpin;
	wxSpinCtrl * m_RPPMinVoxSpin;
	wxSpinCtrl * m_RPPMaxVoxSpin;
	wxSpinCtrl * m_DoSpineSpin;
	wxSpinCtrl * m_DoSpinalSpin;
	wxSpinCtrl * m_DoRibsSpin;
	wxSpinCtrl * m_DoAortaSpin;
	wxSpinCtrl * m_DoLiverSpin;
	wxSpinCtrl * m_DoKidneysSpin;
	wxSpinCtrl * m_DoSpleenSpin;
	
	bool returnedValue;


	//#################### CONSTRUCTORS ####################
public:
	FeatureIdDialog(wxWindow *parent, FIOptions map):	wxDialog(parent, wxID_ANY, wxT("Choose constants"), wxDefaultPosition, wxDefaultSize, wxRESIZE_BORDER)
	{
		
		/*
		 * +--------------+
		 * |+------------+|
		 * ||+--++--++--+||
		 * |||  ||  ||  |||
		 * |||  ||  ||  |||
		 * |||  ||  ||  |||
		 * |||  ||  ||  |||
		 * |||  ||  ||  |||
		 * ||+--++--++--+||
		 * |+------------+|
		 * |+------------+|
		 * ||            ||
		 * |+------------+|
		 * +--------------+
		 */
		
		
		m_parent = parent;
		
		options = map;
		
		//whole dialog
		wxBoxSizer * sizer = new wxBoxSizer(wxVERTICAL);
		
		returnedValue = false;
		
		SetSizer(sizer);
		
		//container for columns
		wxPanel * mainPanel = new wxPanel(this);
		wxSizer * mainSizer = new wxBoxSizer(wxHORIZONTAL);
		
		mainPanel->SetSizer(mainSizer);
		
		// 3 columns
		wxBoxSizer * farLeftSizer = new wxBoxSizer(wxVERTICAL);
		wxBoxSizer * leftSizer = new wxBoxSizer(wxVERTICAL);
		wxBoxSizer * rightSizer = new wxBoxSizer(wxVERTICAL);
		
		wxPanel * farLeft = new wxPanel(mainPanel);
		wxPanel * left = new wxPanel(mainPanel);
		wxPanel * right = new wxPanel(mainPanel);
		
		
		farLeft->SetSizer(farLeftSizer);
		left->SetSizer(leftSizer);
		right->SetSizer(rightSizer);
		
		//Should be checkboxes, but are 0-1 spin boxes
		m_DoSpineSpin = new wxSpinCtrl(farLeft, wxID_ANY, wxString::Format("%i", options["DoSpine"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1, options["DoSpine"]);
		m_DoSpinalSpin = new wxSpinCtrl(farLeft, wxID_ANY, wxString::Format("%i", options["DoSpinal"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1, options["DoSpinal"]);
		m_DoAortaSpin = new wxSpinCtrl(farLeft, wxID_ANY, wxString::Format("%i", options["DoAorta"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1, options["DoAorta"]);
		m_DoLiverSpin = new wxSpinCtrl(farLeft, wxID_ANY, wxString::Format("%i", options["DoLiver"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1, options["DoLiver"]);
		m_DoKidneysSpin = new wxSpinCtrl(farLeft, wxID_ANY, wxString::Format("%i", options["DoKidneys"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1, options["DoKidneys"]);
		m_DoRibsSpin = new wxSpinCtrl(farLeft, wxID_ANY, wxString::Format("%i", options["DoRibs"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1, options["DoRibs"]);
		m_DoSpleenSpin = new wxSpinCtrl(farLeft, wxID_ANY, wxString::Format("%i", options["DoSpleen"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1, options["DoSpleen"]);

		//Constructing Spinboxes
		m_KMaxVoxSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KMaxVox"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 20000, options["KMaxVox"]);
		m_KMinVoxSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KMinVox"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 20000, options["KMinVox"]);
		m_KMaxHSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KMaxH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["KMaxH"]);
		m_KMinHSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KMinH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["KMinH"]);
		m_KMaxAspectSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KMaxAspect"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["KMaxAspect"]);
		m_KMinAspectSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KMinAspect"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["KMinAspect"]);
		m_KSeedToleranceSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KSeedTolerance"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 500, options["KSeedTolerance"]);
		m_KAdjToleranceSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KAdjTolerance"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 500, options["KAdjTolerance"]);
		m_KSpineCloseSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KSpineClose"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 500, options["KSpineClose"]);
		m_KRibDistSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["KRibDist"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 500, options["KRibDist"]);
		m_AMaxVoxSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["AMaxVox"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 20000, options["AMaxVox"]);
		m_AMinVoxSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["AMinVox"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 20000, options["AMinVox"]);
		m_AMaxHSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["AMaxH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["AMaxH"]);
		m_AMinHSpin = new wxSpinCtrl(left, wxID_ANY, wxString::Format("%i", options["AMinH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["AMinH"]);
		m_LMinVoxSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["LMinVox"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 20000, options["LMinVox"]);
		m_LRibDistSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["LRibDist"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 500, options["LRibDist"]);
		m_LMinHSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["LMinH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["LMinH"]);
		m_LSeedToleranceSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["LSeedTolerance"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 500, options["LSeedTolerance"]);
		m_LAdjToleranceSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["LAdjTolerance"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 500, options["LAdjTolerance"]);
		m_LMaxHSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["LMaxH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["LMaxH"]);
		m_LMorphMinHSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["LMorphMinH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["LMorphMinH"]);
		m_RMinSeedHSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["RMinSeedH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["RMinSeedH"]);
		m_RMaxVoxSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["RMaxVox"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 20000, options["RMaxVox"]);
		m_RMinGrowHSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["RMinGrowH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["RMinGrowH"]);
		m_RBoundaryScaleSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["RBoundaryScale"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 20, options["RBoundaryScale"]);
		m_ROutsideMaxHSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["ROutsideMaxH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["ROutsideMaxH"]);
		m_RPPMinHSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["RPPMinH"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -2500, 500, options["RPPMinH"]);
		m_RPPMinVoxSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["RPPMinVox"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 20000, options["RPPMinVox"]);
		m_RPPMaxVoxSpin = new wxSpinCtrl(right, wxID_ANY, wxString::Format("%i", options["RPPMaxVox"]), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 20000, options["RPPMaxVox"]);
		
		//Adding Spinboxes with descriptions
		farLeftSizer->Add(new wxStaticText(farLeft, wxID_ANY, wxT("Do Spine")), 0, wxALIGN_LEFT);
		farLeftSizer->Add(m_DoSpineSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		farLeftSizer->Add(new wxStaticText(farLeft, wxID_ANY, wxT("Do Spinal")), 0, wxALIGN_LEFT);
		farLeftSizer->Add(m_DoSpinalSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		farLeftSizer->Add(new wxStaticText(farLeft, wxID_ANY, wxT("Do Ribs")), 0, wxALIGN_LEFT);
		farLeftSizer->Add(m_DoRibsSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		farLeftSizer->Add(new wxStaticText(farLeft, wxID_ANY, wxT("Do Aorta")), 0, wxALIGN_LEFT);
		farLeftSizer->Add(m_DoAortaSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		farLeftSizer->Add(new wxStaticText(farLeft, wxID_ANY, wxT("Do Liver")), 0, wxALIGN_LEFT);
		farLeftSizer->Add(m_DoLiverSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		farLeftSizer->Add(new wxStaticText(farLeft, wxID_ANY, wxT("Do Kidneys")), 0, wxALIGN_LEFT);
		farLeftSizer->Add(m_DoKidneysSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		farLeftSizer->Add(new wxStaticText(farLeft, wxID_ANY, wxT("Do Spleen")), 0, wxALIGN_LEFT);
		farLeftSizer->Add(m_DoSpleenSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Kidneys")), 0, wxALIGN_LEFT);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Max Voxels Per Slice")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KMaxVoxSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Min Voxels Per SLice")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KMinVoxSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Max Mean HoundsField Value")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KMaxHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Min Mean HoundsField Value")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KMinHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Max Aspect Ratio (x10)")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KMaxAspectSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Min Aspect Ratio (x10)")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KMinAspectSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Seed Tolerance")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KSeedToleranceSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Adjacent Tolerance")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KAdjToleranceSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Closeness to spine")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KSpineCloseSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Distance from ribs")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_KRibDistSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Aorta")), 0, wxALIGN_LEFT);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Max Voxels Per Slice")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_AMaxVoxSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Min Voxels Per SLice")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_AMinVoxSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Max Mean HoundsField Value")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_AMaxHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		leftSizer->Add(new wxStaticText(left, wxID_ANY, wxT("Min Mean HoundsField Value")), 0, wxALIGN_LEFT);
		leftSizer->Add(m_AMinHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		
		
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Liver")), 0, wxALIGN_LEFT);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Min Voxels Per SLice")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_LMinVoxSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Distance from ribs")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_LRibDistSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Min Mean HoundsField Value")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_LMinHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Seed Tolerance")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_LSeedToleranceSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Adjacent Tolerance")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_LAdjToleranceSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Max Mean HoundsField Value")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_LMaxHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Min Mean HoundsField Value (for Morph Closing)")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_LMorphMinHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Ribs")), 0, wxALIGN_LEFT);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Min Mean HoundsField Value (Seed)")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_RMinSeedHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Max Voxels Per Slice")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_RMaxVoxSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Min Mean HoundsField Value (Growing)")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_RMinGrowHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Radius Scale Factor (x10)")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_RBoundaryScaleSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Max HoundsField Value Outside Body")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_ROutsideMaxHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Ribs (Post Processing)")), 0, wxALIGN_LEFT);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Min Mean HoundsField Value")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_RPPMinHSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Min Voxels Per Slice")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_RPPMinVoxSpin, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(new wxStaticText(right, wxID_ANY, wxT("Max Voxels Per Slice")), 0, wxALIGN_LEFT);
		rightSizer->Add(m_RPPMaxVoxSpin, 0, wxALIGN_CENTER_HORIZONTAL);

		
		//sizer->AddSpacer(10);
		
		wxPanel *bottomPanel = new wxPanel(this);
		wxBoxSizer *bottom = new wxBoxSizer(wxHORIZONTAL);
		bottomPanel->SetSizer(bottom);
		bottom->Fit(bottomPanel);
		
		wxButton* loadButton = new wxButton(bottomPanel, BUTTONID_LOAD, wxT("Load"));
		wxButton* saveButton = new wxButton(bottomPanel, BUTTONID_SAVE, wxT("Save"));
		wxButton* okButton = new wxButton(bottomPanel, BUTTONID_OK, wxT("OK"));
		wxButton* cancelButton = new wxButton(bottomPanel, BUTTONID_CANCEL, wxT("Cancel"));
		
		bottom->Add(saveButton, 0, wxALIGN_CENTER_VERTICAL);
		bottom->Add(loadButton, 0, wxALIGN_CENTER_VERTICAL);
		bottom->Add(okButton, 0, wxALIGN_CENTER_VERTICAL);
		bottom->Add(cancelButton, 0, wxALIGN_CENTER_VERTICAL);
		
		sizer->Add(mainPanel, 0, wxALIGN_CENTER_HORIZONTAL);
		sizer->Add(bottomPanel, 0, wxALIGN_CENTER_HORIZONTAL);
		
		mainSizer->Add(farLeft, 0, wxALIGN_CENTER_VERTICAL);
		mainSizer->Add(left, 0, wxALIGN_CENTER_VERTICAL);
		mainSizer->Add(right, 0, wxALIGN_CENTER_VERTICAL);
		
		farLeftSizer->Fit(farLeft);
		leftSizer->Fit(left);
		rightSizer->Fit(right);
		
		mainSizer->Fit(mainPanel);
		
		sizer->Fit(this);
		
	
	}
	
	//#################### PUBLIC METHODS ###################
	FIOptions* get_options() {
		return &options;
	}

	//#################### PRIVATE METHODS ####################

private:
	
	/**
	 * 
	 * Some identifiers rely on others - checks these dependecies:
	 * 
	 * [ --> := "Required by"]
	 * 
	 * Spine --> Ribs --> Kidneys
	 *    |        |
	 *    |        +----> Liver
	 *    |
	 *    +----> Spinal Cord --> Aorta
	 * 
	 */
	bool check_dependencies() {
		
		bool valid = true;
		
		if (options["DoRibs"] == 1 && options["DoSpine"] != 1) { valid = false; options["DoRibs"] = 0;};
		
		if (options["DoSpinal"] == 1 && options["DoSpine"] != 1) { valid = false; ; options["DoSpinal"] = 0;};
		
		if (options["DoAorta"] == 1 && options["DoSpinal"] != 1) { valid = false; ; options["DoAorta"] = 0;};
			
		if (options["DoKidneys"] == 1 && options["DoRibs"] != 1) { valid = false; options["DoKidneys"] = 0;};
		
		if (options["DoLiver"] == 1 && options["DoRibs"] != 1) { valid = false; options["DoLiver"] = 0;};
		
		return valid;
	}
	
	/**
	 * refreshes spinbox values
	 */
	void reload() {
		m_KMaxVoxSpin->SetValue(options["KMaxVox"]);
		m_KMinVoxSpin->SetValue(options["KMinVox"]);
		m_KMaxHSpin->SetValue(options["KMaxH"]);
		m_KMinHSpin->SetValue(options["KMinH"]);
		m_KMaxAspectSpin->SetValue(options["KMaxAspect"]);
		m_KMinAspectSpin->SetValue(options["KMinAspect"]);
		m_KSeedToleranceSpin->SetValue(options["KSeedTolerance"]);
		m_KAdjToleranceSpin->SetValue(options["KAdjTolerance"]);
		m_KSpineCloseSpin->SetValue(options["KSpineClose"]);
		m_KRibDistSpin->SetValue(options["KRibDist"]);
		m_AMaxVoxSpin->SetValue(options["AMaxVox"]);
		m_AMinVoxSpin->SetValue(options["AMinVox"]);
		m_AMaxHSpin->SetValue(options["AMaxH"]);
		m_AMinHSpin->SetValue(options["AMinH"]);
		m_LMinVoxSpin->SetValue(options["LMinVox"]);
		m_LSeedToleranceSpin->SetValue(options["LSeedTolerance"]);
		m_LAdjToleranceSpin->SetValue(options["LAdjTolerance"]);
		m_LRibDistSpin->SetValue(options["LRibDist"]);
		m_LMinHSpin->SetValue(options["LMinH"]);
		m_LMaxHSpin->SetValue(options["LMaxH"]);
		m_LMorphMinHSpin->SetValue(options["LMorphMinH"]);
		m_RMinSeedHSpin->SetValue(options["RMinSeedH"]);
		m_RMaxVoxSpin->SetValue(options["RMaxVox"]);
		m_RMinGrowHSpin->SetValue(options["RMinGrowH"]);
		m_RBoundaryScaleSpin->SetValue(options["RBoundaryScale"]);
		m_ROutsideMaxHSpin->SetValue(options["ROutsideMaxH"]);
		m_RPPMinHSpin->SetValue(options["RPPMinH"]);
		m_RPPMinVoxSpin->SetValue(options["RPPMinVox"]);
		m_RPPMaxVoxSpin->SetValue(options["RPPMaxVox"]);
		m_DoSpineSpin->SetValue(options["DoSpine"]);
		m_DoSpinalSpin->SetValue(options["DoSpinal"]);
		m_DoRibsSpin->SetValue(options["DoRibs"]);
		m_DoAortaSpin->SetValue(options["DoAorta"]);
		m_DoLiverSpin->SetValue(options["DoLiver"]);
		m_DoKidneysSpin->SetValue(options["DoKidneys"]);
		m_DoSpleenSpin->SetValue(options["DoSpleen"]);
	}

	/**
	 * Puts the spinbox values into the map of options
	 */
	void construct_options() {
		
		options[std::string("KMaxVox")] = m_KMaxVoxSpin->GetValue();
		options[std::string("KMinVox")] = m_KMinVoxSpin->GetValue();
		options[std::string("KMaxH")] = m_KMaxHSpin->GetValue();
		options[std::string("KMinH")] = m_KMinHSpin->GetValue();
		options[std::string("KMaxAspect")] = m_KMaxAspectSpin->GetValue();
		options[std::string("KMinAspect")] = m_KMinAspectSpin->GetValue();
		options[std::string("KSeedTolerance")] = m_KSeedToleranceSpin->GetValue();
		options[std::string("KAdjTolerance")] = m_KAdjToleranceSpin->GetValue();
		options[std::string("KSpineClose")] = m_KSpineCloseSpin->GetValue();
		options[std::string("KRibDist")] = m_KRibDistSpin->GetValue();
		options[std::string("AMaxVox")] = m_AMaxVoxSpin->GetValue();
		options[std::string("AMinVox")] = m_AMinVoxSpin->GetValue();
		options[std::string("AMaxH")] = m_AMaxHSpin->GetValue();
		options[std::string("AMinH")] = m_AMinHSpin->GetValue();
		options[std::string("LMinVox")] = m_LMinVoxSpin->GetValue();
		options[std::string("LRibDist")] = m_LRibDistSpin->GetValue();
		options[std::string("LMinH")] = m_LMinHSpin->GetValue();
		options[std::string("LSeedTolerance")] = m_LSeedToleranceSpin->GetValue();
		options[std::string("LAdjTolerance")] = m_LAdjToleranceSpin->GetValue();
		options[std::string("LMaxH")] = m_LMaxHSpin->GetValue();
		options[std::string("LMorphMinH")] = m_LMorphMinHSpin->GetValue();
		options[std::string("RMinSeedH")] = m_RMinSeedHSpin->GetValue();
		options[std::string("RMaxVox")] = m_RMaxVoxSpin->GetValue();
		options[std::string("RMinGrowH")] = m_RMinGrowHSpin->GetValue();
		options[std::string("RBoundaryScale")] = m_RBoundaryScaleSpin->GetValue();
		options[std::string("ROutsideMaxH")] = m_ROutsideMaxHSpin->GetValue();
		options[std::string("RPPMinH")] = m_RPPMinHSpin->GetValue();
		options[std::string("RPPMinVox")] = m_RPPMinVoxSpin->GetValue();
		options[std::string("RPPMaxVox")] = m_RPPMaxVoxSpin->GetValue();
		options[std::string("DoSpine")] = m_DoSpineSpin->GetValue();
		options[std::string("DoSpinal")] = m_DoSpinalSpin->GetValue();
		options[std::string("DoRibs")] = m_DoRibsSpin->GetValue();
		options[std::string("DoAorta")] = m_DoAortaSpin->GetValue();
		options[std::string("DoLiver")] = m_DoLiverSpin->GetValue();
		options[std::string("DoKidneys")] = m_DoKidneysSpin->GetValue();
		options[std::string("DoSpleen")] = m_DoSpleenSpin->GetValue();
	}
	
public:

	void OnOK(wxCommandEvent&) {
		construct_options();
		
		if (check_dependencies()) {
			returnedValue = true;
			Close();
		}
	}
	
	void OnCancel(wxCommandEvent&) {
		
		Close();
	}
	
	void OnLoad(wxCommandEvent&) {
		
		wxFileDialog* loadDialog = new wxFileDialog(
		this, _("Choose a file to load"), wxEmptyString, wxEmptyString, 
		_("Text files (*.txt)|*.txt"),
		wxFD_OPEN, wxDefaultPosition);
 
		if (loadDialog->ShowModal() == wxID_OK) // if the user click "Open" instead of "Cancel"
		{
			const char * path = (loadDialog->GetPath()).mb_str();
			options.load(path);
			
		}
	
		// Clean up after ourselves
		loadDialog->Destroy();
		reload();
	}
	
	void OnSave(wxCommandEvent&) {

		construct_options();
		
		wxFileDialog* saveDialog = new wxFileDialog(
		this, _("Choose a file to save to"), wxEmptyString, wxEmptyString, 
		_("Text files (*.txt)|*.txt"),
		wxFD_SAVE, wxDefaultPosition);
 
		if (saveDialog->ShowModal() == wxID_OK) // if the user click "Open" instead of "Cancel"
		{
			const char * path = (saveDialog->GetPath()).mb_str();
			options.save(path);
			
		}
	
		// Clean up after ourselves
		saveDialog->Destroy();
		
	}

	bool returnedSomething() {
		return returnedValue;
	}
	
	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
	
};
//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(FeatureIdDialog, wxDialog)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_OK, FeatureIdDialog::OnOK)
	EVT_BUTTON(BUTTONID_CANCEL, FeatureIdDialog::OnCancel)
	EVT_BUTTON(BUTTONID_SAVE, FeatureIdDialog::OnSave)
	EVT_BUTTON(BUTTONID_LOAD, FeatureIdDialog::OnLoad)
END_EVENT_TABLE()

}

using mp_FeatureIdDialog::FeatureIdDialog;

}

#endif
