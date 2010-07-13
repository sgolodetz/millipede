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

template <typename LeafLayer, typename BranchLayer, typename Feature>
class LabelImageCreator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<int,3> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;
	typedef typename VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;
	typedef VolumeIPFMultiFeatureSelection<LeafLayer,BranchLayer,Feature> VolumeIPFMultiFeatureSelectionT;
	typedef boost::shared_ptr<const VolumeIPFMultiFeatureSelectionT> VolumeIPFMultiFeatureSelection_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	DataHook<LabelImagePointer> m_labellingHook;
	VolumeIPFMultiFeatureSelection_CPtr m_multiFeatureSelection;
	int m_voxelCount;

	//#################### CONSTRUCTORS ####################
public:
	explicit LabelImageCreator(const VolumeIPFMultiFeatureSelection_CPtr& multiFeatureSelection)
	:	m_multiFeatureSelection(multiFeatureSelection)
	{
		itk::Size<3> volumeSize = multiFeatureSelection->volume_ipf()->volume_size();
		m_voxelCount = volumeSize[0] * volumeSize[1] * volumeSize[2];
	}

	//#################### PUBLIC METHODS ####################
public:
	const DataHook<LabelImagePointer>& get_labelling_hook() const
	{
		return m_labellingHook;
	}

	int length() const
	{
		return m_voxelCount + 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl()
	{
		set_status("Creating label image...");

		VolumeIPF_CPtr volumeIPF = m_multiFeatureSelection->volume_ipf();
		itk::Size<3> volumeSize = volumeIPF->volume_size();
		LabelImagePointer labelling = ITKImageUtil::make_image<int>(volumeSize);

		// Note: An index has signed values, whereas a size has unsigned ones. Doing this avoids signed/unsigned mismatch warnings.
		itk::Index<3> size = ITKImageUtil::make_index_from_size(volumeSize);

		itk::Index<3> index;
		for(index[2]=0; index[2]<size[2]; ++index[2])
			for(index[1]=0; index[1]<size[1]; ++index[1])
				for(index[0]=0; index[0]<size[0]; ++index[0])
				{
					PFNodeID n(0, volumeIPF->leaf_of_position(index));
					std::vector<Feature> features = m_multiFeatureSelection->features_of(n);

					// Assume that (a) features can be mapped straightforwardly to ints and (b) the first feature is the most important.
					int value = !features.empty() ? features[0] + 1 : 0;
					labelling->SetPixel(index, value);

					increment_progress();
				}

		m_labellingHook.set(labelling);
		increment_progress();
	}
};

}

#endif
