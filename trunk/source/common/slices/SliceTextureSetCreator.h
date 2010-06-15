/***
 * millipede: SliceTextureSetCreator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SLICETEXTURESETCREATOR
#define H_MILLIPEDE_SLICETEXTURESETCREATOR

#include <itkExtractImageFilter.h>

#include <common/io/util/OSSWrapper.h>
#include <common/jobs/CompositeJobs.h>
#include <common/jobs/SimpleJobs.h>
#include <common/textures/TextureFactory.h>
#include "SliceTextureSet.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class Texture> Texture_Ptr;

template <typename TPixel>
class SliceTextureSetCreator : public CompositeJob
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<TPixel,2> Image2D;
	typedef itk::Image<TPixel,3> Image3D;

	//#################### NESTED CLASSES ####################
private:
	struct ExtractSliceJob : SimpleJob
	{
		typename Image3D::Pointer volumeImage;
		SliceOrientation ori;
		int sliceIndex;
		typename Image2D::Pointer& sliceImage;

		ExtractSliceJob(const typename Image3D::Pointer& volumeImage_, SliceOrientation ori_, int sliceIndex_, typename Image2D::Pointer& sliceImage_)
		:	volumeImage(volumeImage_), ori(ori_), sliceIndex(sliceIndex_), sliceImage(sliceImage_)
		{}

		void execute()
		{
			set_status(OSSWrapper() << "Extracting slice " << sliceIndex << "...");

			typename Image3D::SizeType volumeSize = volumeImage->GetLargestPossibleRegion().GetSize();

			typedef itk::ExtractImageFilter<Image3D,Image2D> Extractor;
			typename Extractor::Pointer extractor = Extractor::New();
			extractor->SetInput(volumeImage);

			typename Image3D::IndexType index;
			typename Image3D::SizeType size;
			typename Image3D::RegionType region;

			for(int i=0; i<3; ++i)
			{
				index[i] = 0;
				size[i] = volumeSize[i];
			}
			index[ori] = sliceIndex;
			size[ori] = 0;

			region.SetIndex(index);
			region.SetSize(size);
			extractor->SetExtractionRegion(region);

			if(is_aborted()) return;
			extractor->Update();
			if(is_aborted()) return;
			sliceImage = extractor->GetOutput();
			if(is_aborted()) return;

			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	struct CreateTextureJob : SimpleJob
	{
		const typename Image2D::Pointer& sliceImage;
		int sliceIndex;
		Texture_Ptr& texture;

		CreateTextureJob(const typename Image2D::Pointer& sliceImage_, int sliceIndex_, Texture_Ptr& texture_)
		:	sliceImage(sliceImage_), sliceIndex(sliceIndex_), texture(texture_)
		{}

		void execute()
		{
			set_status(OSSWrapper() << "Creating texture for slice " << sliceIndex << "...");
			texture = TextureFactory::create_texture(sliceImage);
			if(is_aborted()) return;
			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	struct TextureSetFillerJob : SimpleJob
	{
		SliceTextureSet_Ptr sliceTextureSet;
		SliceOrientation ori;
		const std::vector<Texture_Ptr>& textures;

		TextureSetFillerJob(const SliceTextureSet_Ptr& sliceTextureSet_, SliceOrientation ori_, const std::vector<Texture_Ptr>& textures_)
		:	sliceTextureSet(sliceTextureSet_), ori(ori_), textures(textures_)
		{}

		void execute()
		{
			set_status("Filling texture set...");
			sliceTextureSet->set_textures(ori, textures);
			if(is_aborted()) return;
			set_finished();
		}

		int length() const
		{
			return 1;
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	typename Image2D::Pointer m_sliceImage;
	std::vector<Texture_Ptr> m_textures;

	//#################### CONSTRUCTORS ####################
public:
	SliceTextureSetCreator(const typename Image3D::Pointer& volumeImage, SliceOrientation ori, const SliceTextureSet_Ptr& sliceTextureSet)
	{
		typename Image3D::SizeType volumeSize = volumeImage->GetLargestPossibleRegion().GetSize();

		m_textures.resize(volumeSize[ori]);

		for(unsigned int i=0; i<volumeSize[ori]; ++i)
		{
			add_subjob(new ExtractSliceJob(volumeImage, ori, i, m_sliceImage));
			add_main_thread_subjob(new CreateTextureJob(m_sliceImage, i, m_textures[i]));
		}

		add_subjob(new TextureSetFillerJob(sliceTextureSet, ori, m_textures));
	}
};

}

#endif
