/***
 * millipede: FeatureIdentifier.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FEATUREIDENTIFIER
#define H_MILLIPEDE_FEATUREIDENTIFIER

#ifdef _MSC_VER
	// Disable the spurious "inherits * via dominance" warnings that VC++ will throw out - the virtual inheritance being used here is entirely intentional.
	#pragma warning(disable:4250)
#endif

#include <common/jobs/DataHook.h>
#include <common/jobs/Job.h>
#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMVolume> DICOMVolume_CPtr;

class FeatureIdentifier : public virtual Job
{
	//#################### TYPEDEFS ####################
protected:
	typedef DICOMImageBranchLayer BranchLayer;
	typedef AbdominalFeature::Enum Feature;
	typedef DICOMImageLeafLayer LeafLayer;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;
	typedef VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_Ptr;

	typedef VolumeIPFT::BranchNodeConstIterator BranchNodeConstIterator;
	typedef VolumeIPFT::BranchProperties BranchProperties;
	typedef VolumeIPFT::LeafNodeConstIterator LeafNodeConstIterator;
	typedef VolumeIPFT::LeafProperties LeafProperties;

	//#################### PRIVATE VARIABLES ####################
private:
	DICOMVolume_CPtr m_dicomVolume;
	DataHook<VolumeIPFMultiFeatureSelection_Ptr> m_mfsHook;
	VolumeIPF_Ptr m_volumeIPF;

	//#################### CONSTRUCTORS ####################
public:
	FeatureIdentifier(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF);

	//#################### PUBLIC METHODS ####################
public:
	const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& get_mfs_hook() const;
	const VolumeIPFMultiFeatureSelection_Ptr& get_multi_feature_selection() const;
	void set_mfs_hook(const DataHook<VolumeIPFMultiFeatureSelection_Ptr>& mfsHook);

	//#################### PROTECTED METHODS ####################
protected:
	DICOMVolume_CPtr dicom_volume() const;
	VolumeIPF_CPtr volume_ipf() const;

	template <typename Pred>
	std::list<PFNodeID> filter_branch_nodes(Pred pred)
	{
		std::list<PFNodeID> result;
		for(int layer=1, highestLayer=m_volumeIPF->highest_layer(); layer<=highestLayer; ++layer)
		{
			for(BranchNodeConstIterator it=m_volumeIPF->branch_nodes_cbegin(layer), iend=m_volumeIPF->branch_nodes_cend(layer); it!=iend; ++it)
			{
				PFNodeID node(layer, it.index());
				if(pred(node, it->properties()))
				{
					result.push_back(node);
				}
			}
		}
		return result;
	}
};

}

#endif
