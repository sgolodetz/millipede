/***
 * millipede: SliceTextureSetFiller.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SLICETEXTURESETFILLER
#define H_MILLIPEDE_SLICETEXTURESETFILLER

#include <itkExtractImageFilter.h>

#include <common/io/util/OSSWrapper.h>
#include <common/jobs/CompositeJob.h>
#include <common/jobs/DataHook.h>
#include <common/jobs/SimpleJob.h>
#include <common/textures/TextureFactory.h>
#include "SliceTextureSet.h"

namespace mp {

template <typename TPixel>
class SliceTextureSetFiller : public CompositeJob
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<TPixel,2> Image2D;
	typedef itk::Image<TPixel,3> Image3D;

	//#################### NESTED CLASSES ####################
private:
	struct ExtractSliceJob : SimpleJob
	{
		SliceOrientation ori;
		int sliceIndex;
		DataHook<typename Image2D::Pointer> sliceImageHook;
		boost::shared_ptr<DataHook<typename Image3D::Pointer> > volumeImageHook;

		ExtractSliceJob(SliceOrientation ori_, int sliceIndex_)
		:	ori(ori_), sliceIndex(sliceIndex_)
		{}

		void execute_impl()
		{
			set_status(OSSWrapper() << "Extracting slice " << sliceIndex << "...");

			typename Image3D::SizeType volumeSize = volumeImageHook->get()->GetLargestPossibleRegion().GetSize();

			typedef itk::ExtractImageFilter<Image3D,Image2D> Extractor;
			typename Extractor::Pointer extractor = Extractor::New();
			extractor->SetInput(volumeImageHook->get());

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
			sliceImageHook.set(extractor->GetOutput());
			if(is_aborted()) return;
		}

		int length() const
		{
			return 1;
		}
	};

	struct CreateTextureJob : SimpleJob
	{
		DataHook<typename Image2D::Pointer> sliceImageHook;
		int sliceIndex;
		DataHook<Texture_Ptr> textureHook;

		explicit CreateTextureJob(int sliceIndex_)
		:	sliceIndex(sliceIndex_)
		{}

		void execute_impl()
		{
			set_status(OSSWrapper() << "Creating texture for slice " << sliceIndex << "...");
			textureHook.set(TextureFactory::create_texture(sliceImageHook.get()));
			if(is_aborted()) return;
		}

		int length() const
		{
			return 1;
		}
	};

	struct TextureSetFillerJob : SimpleJob
	{
		SliceOrientation ori;
		SliceTextureSet_Ptr sliceTextureSet;
		std::vector<DataHook<Texture_Ptr> > textureHooks;

		TextureSetFillerJob(SliceOrientation ori_, const SliceTextureSet_Ptr& sliceTextureSet_)
		:	ori(ori_), sliceTextureSet(sliceTextureSet_)
		{}

		void execute_impl()
		{
			set_status("Filling texture set...");

			std::vector<Texture_Ptr> textures;
			for(size_t i=0, size=textureHooks.size(); i<size; ++i)
			{
				textures.push_back(textureHooks[i].get());
			}
			sliceTextureSet->set_textures(ori, textures);

			if(is_aborted()) return;
		}

		int length() const
		{
			return 1;
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	boost::shared_ptr<DataHook<typename Image3D::Pointer> > m_volumeImageHook;

	//#################### CONSTRUCTORS ####################
public:
	SliceTextureSetFiller(SliceOrientation ori, const typename Image3D::SizeType& volumeSize, const SliceTextureSet_Ptr& sliceTextureSet)
	:	m_volumeImageHook(new DataHook<typename Image3D::Pointer>)
	{
		TextureSetFillerJob *textureSetFillerJob = new TextureSetFillerJob(ori, sliceTextureSet);
		for(unsigned int i=0; i<volumeSize[ori]; ++i)
		{
			ExtractSliceJob *extractSliceJob = new ExtractSliceJob(ori, i);
			CreateTextureJob *createTextureJob = new CreateTextureJob(i);

			extractSliceJob->volumeImageHook = m_volumeImageHook;
			createTextureJob->sliceImageHook = extractSliceJob->sliceImageHook;
			textureSetFillerJob->textureHooks.push_back(createTextureJob->textureHook);

			add_subjob(extractSliceJob);
			add_main_thread_subjob(createTextureJob);
		}

		add_subjob(textureSetFillerJob);
	}

	void set_volume_image(const typename Image3D::Pointer& volumeImage)
	{
		m_volumeImageHook->set(volumeImage);
	}

	void set_volume_image_hook(const DataHook<typename Image3D::Pointer>& volumeImageHook)
	{
		*m_volumeImageHook = volumeImageHook;
	}
};

}

#endif
