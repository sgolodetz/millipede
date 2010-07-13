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
	itk::Size<3> m_labellingSize;
	VolumeIPFMultiFeatureSelection_CPtr m_multiFeatureSelection;
	int m_taskCount;

	//#################### CONSTRUCTORS ####################
public:
	explicit LabelImageCreator(const VolumeIPFMultiFeatureSelection_CPtr& multiFeatureSelection)
	:	m_multiFeatureSelection(multiFeatureSelection)
	{
		m_labellingSize = multiFeatureSelection->volume_ipf()->volume_size();
		for(int i=0; i<3; ++i) m_labellingSize[i] += 2;		// add a single voxel border around the labelling (otherwise we can't visualize single slices etc.)
		m_taskCount = m_labellingSize[0] * m_labellingSize[1] * m_labellingSize[2];
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
		return m_taskCount + 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl()
	{
		set_status("Creating label image...");

		VolumeIPF_CPtr volumeIPF = m_multiFeatureSelection->volume_ipf();
		LabelImagePointer labelling = ITKImageUtil::make_image<int>(m_labellingSize);

		// Note: An index has signed values, whereas a size has unsigned ones. Doing this avoids signed/unsigned mismatch warnings.
		itk::Index<3> size = ITKImageUtil::make_index_from_size(m_labellingSize);

		itk::Index<3> index;
		for(index[2]=0; index[2]<size[2]; ++index[2])
			for(index[1]=0; index[1]<size[1]; ++index[1])
				for(index[0]=0; index[0]<size[0]; ++index[0])
				{
					if(index[0] != 0 && index[1] != 0 && index[2] != 0 &&
					   index[0] != size[0]-1 && index[1] != size[1]-1 && index[2] != size[2]-1)
					{
						itk::Index<3> position = {{index[0]-1, index[1]-1, index[2]-1}};
						PFNodeID n(0, volumeIPF->leaf_of_position(position));
						std::vector<Feature> features = m_multiFeatureSelection->features_of(n);

						// Assume that (a) features can be mapped straightforwardly to ints and (b) the first feature is the most important.
						int value = !features.empty() ? features[0] + 1 : 0;
						labelling->SetPixel(index, value);
					}
					else
					{
						labelling->SetPixel(index, 0);
					}

					increment_progress();
				}

		m_labellingHook.set(labelling);
		increment_progress();
	}
};

}

#endif
