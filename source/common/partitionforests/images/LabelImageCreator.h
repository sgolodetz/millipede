/***
 * millipede: LabelImageCreator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_LABELIMAGECREATOR
#define H_MILLIPEDE_LABELIMAGECREATOR

#include <itkImage.h>

#include <common/jobs/DataHook.h>
#include <common/jobs/SimpleJob.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

template <typename Feature>
int feature_to_int(const Feature& feature)
{
	// Note: This obviously won't work for all feature types, but it will be fine for enums.
	return feature + 1;
}

template <typename LeafLayer, typename BranchLayer, typename Feature>
class LabelImageCreator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<int,3> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;
	typedef PartitionForestSelection<LeafLayer,BranchLayer> PartitionForestSelectionT;
	typedef boost::shared_ptr<const PartitionForestSelectionT> PartitionForestSelection_CPtr;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;
	typedef VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<const VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	DataHook<LabelImagePointer> m_labellingHook;
	itk::Size<3> m_labellingSize;
	VolumeIPFMultiFeatureSelection_CPtr m_multiFeatureSelection;

	//#################### CONSTRUCTORS ####################
public:
	explicit LabelImageCreator(const VolumeIPFMultiFeatureSelection_CPtr& multiFeatureSelection)
	:	m_multiFeatureSelection(multiFeatureSelection)
	{
		m_labellingSize = multiFeatureSelection->volume_ipf()->volume_size();
		for(int i=0; i<3; ++i) m_labellingSize[i] += 2;		// add a single voxel border around the labelling (otherwise we can't visualize single slices etc.)
	}

	//#################### PUBLIC METHODS ####################
public:
	const DataHook<LabelImagePointer>& get_labelling_hook() const
	{
		return m_labellingHook;
	}

	const itk::Size<3>& labelling_size() const
	{
		return m_labellingSize;
	}

	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl()
	{
		set_status("Creating label image...");

		VolumeIPF_CPtr volumeIPF = m_multiFeatureSelection->volume_ipf();
		LabelImagePointer labelling = ITKImageUtil::make_image<int>(m_labellingSize);
		labelling->FillBuffer(0);

		for(Feature f=enum_begin<Feature>(), end=enum_end<Feature>(); f!=end; ++f)
		{
			if(!m_multiFeatureSelection->has_selection(f)) continue;

			PartitionForestSelection_CPtr selection = m_multiFeatureSelection->selection(f);
			int value = feature_to_int(f);

			typedef typename PartitionForestSelectionT::NodeConstIterator Iter;
			for(Iter it=selection->nodes_cbegin(), iend=selection->nodes_cend(); it!=iend; ++it)
			{
				std::deque<int> receptiveRegion = volumeIPF->receptive_region_of(*it);
				for(std::deque<int>::const_iterator jt=receptiveRegion.begin(), jend=receptiveRegion.end(); jt!=jend; ++jt)
				{
					itk::Index<3> position = volumeIPF->position_of_leaf(*jt);
					itk::Index<3> index = {{position[0]+1, (m_labellingSize[1]-2) - position[1], position[2]+1}};	// note the flipped y axis
					labelling->SetPixel(index, value);
				}
			}
		}

		m_labellingHook.set(labelling);
	}
};

}

#endif
