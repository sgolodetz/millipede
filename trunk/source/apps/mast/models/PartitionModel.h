/***
 * millipede: PartitionModel.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONMODEL
#define H_MILLIPEDE_PARTITIONMODEL

#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <common/partitionforests/images/CTImageBranchLayer.h>
#include <common/partitionforests/images/CTImageLeafLayer.h>
#include <common/partitionforests/images/IPFGrid.h>
#include <common/partitionforests/images/IPFSelectionGrid.h>
#include <common/slices/SliceOrientation.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class DICOMVolume> DICOMVolume_Ptr;
typedef shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;
typedef shared_ptr<class SliceTextureSet> SliceTextureSet_Ptr;
typedef shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;

class PartitionModel
{
	//#################### NESTED CLASSES ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void model_changed() = 0;
	};

	struct ViewLocation
	{
		int x, y, z, layer;

		ViewLocation(int x_, int y_, int z_, int layer_)
		:	x(x_), y(y_), z(z_), layer(layer_)
		{}
	};

	//#################### TYPEDEFS ####################
public:
	typedef PartitionForest<CTImageLeafLayer,CTImageBranchLayer> IPF;
	typedef IPFGrid<IPF> IPFGridT;
	typedef boost::shared_ptr<IPFGridT> IPFGrid_Ptr;
	typedef boost::shared_ptr<const IPFGridT> IPFGrid_CPtr;

	typedef PartitionForestSelection<CTImageLeafLayer,CTImageBranchLayer> IPFSelection;
	typedef IPFSelectionGrid<IPFSelection> IPFSelectionGridT;
	typedef boost::shared_ptr<IPFSelectionGridT> IPFSelectionGrid_Ptr;
	typedef boost::shared_ptr<const IPFSelectionGridT> IPFSelectionGrid_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	SliceTextureSet_Ptr m_dicomTextureSet;
	DICOMVolume_Ptr m_dicomVolume;
	IPFGrid_Ptr m_ipfGrid;
	IPFSelectionGrid_Ptr m_selectionGrid;
	std::vector<SliceTextureSet_Ptr> m_partitionTextureSets;
	SliceOrientation m_sliceOrientation;
	ViewLocation m_viewLocation;			// view location in terms of the volume only (not based on actual slice numbers)

	std::vector<Listener*> m_listeners;

	//#################### CONSTRUCTORS ####################
public:
	PartitionModel(const DICOMVolume_Ptr& volume, const ViewLocation& loc, SliceOrientation ori);

	//#################### PUBLIC METHODS ####################
public:
	void add_listener(Listener *listener);
	SliceTextureSet_CPtr dicom_texture_set() const;
	DICOMVolume_CPtr dicom_volume() const;
	const IPFGrid_Ptr& ipf_grid();
	IPFGrid_CPtr ipf_grid() const;
	SliceTextureSet_CPtr partition_texture_set(int layer) const;
	const IPFSelectionGrid_Ptr& selection_grid();
	IPFSelectionGrid_CPtr selection_grid() const;
	void set_dicom_texture_set(const SliceTextureSet_Ptr& dicomTextureSet);
	void set_ipf_grid(const IPFGrid_Ptr& ipfGrid);
	void set_partition_texture_sets(const std::vector<SliceTextureSet_Ptr>& partitionTextureSets);
	void set_slice_orientation(SliceOrientation ori);
	void set_view_location(const ViewLocation& loc);
	SliceOrientation slice_orientation() const;
	const ViewLocation& view_location() const;

	//#################### PRIVATE METHODS ####################
private:
	void alert_listeners();
};

//#################### TYPEDEFS ####################
typedef shared_ptr<PartitionModel> PartitionModel_Ptr;
typedef shared_ptr<const PartitionModel> PartitionModel_CPtr;

}

#endif
