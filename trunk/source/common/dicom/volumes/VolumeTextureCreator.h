/***
 * millipede: VolumeTextureCreator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMETEXTURECREATOR
#define H_MILLIPEDE_VOLUMETEXTURECREATOR

#include <itkExtractImageFilter.h>

#include <common/jobs/CompositeJob.h>
#include <common/jobs/SimpleJob.h>
#include <common/textures/TextureFactory.h>
#include "SliceOrientation.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class Texture> Texture_Ptr;

template <typename TPixel>
class VolumeTextureCreator : public CompositeJob
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<TPixel,2> Image2D;
	typedef itk::Image<TPixel,3> Image3D;

	//#################### NESTED CLASSES ####################
private:
	struct ExtractSliceJob : SimpleJob
	{
		typename Image3D::ConstPointer volumeImage;
		SliceOrientation ori;
		int sliceIndex;
		typename Image2D::Pointer& sliceImage;

		ExtractSliceJob(const typename Image3D::ConstPointer& volumeImage_, SliceOrientation ori_, int sliceIndex_, typename Image2D::Pointer& sliceImage_)
		:	volumeImage(volumeImage_), ori(ori_), sliceIndex(sliceIndex_), sliceImage(sliceImage_)
		{}

		void execute()
		{
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

			extractor->Update();
			sliceImage = extractor->GetOutput();
		}

		int length() const
		{
			return 1;
		}
	};

	struct CreateTextureJob : SimpleJob
	{
		typename Image2D::Pointer sliceImage;
		Texture_Ptr& texture;

		CreateTextureJob(const typename Image2D::Pointer& sliceImage_, Texture_Ptr& texture_)
		:	sliceImage(sliceImage_), texture(texture_)
		{}

		void execute()
		{
			texture = TextureFactory::create_texture(sliceImage);
		}

		int length() const
		{
			return 1;
		}
	};

	//#################### CONSTRUCTORS ####################
public:
	VolumeTextureCreator(const typename Image3D::ConstPointer& volumeImage, SliceOrientation ori/*, const VolumeTextureSet_Ptr& volumeTextureSet*/)
	{
		typename Image3D::SizeType volumeSize = volumeImage->GetLargestPossibleRegion().GetSize();

		std::vector<Texture_Ptr> textures(volumeSize[ori]);

		typename Image2D::Pointer sliceImage;
		for(unsigned int i=0; i<volumeSize[ori]; ++i)
		{
			add_subjob(new ExtractSliceJob(volumeImage, ori, i, sliceImage));
			add_main_thread_subjob(new CreateTextureJob(sliceImage, textures[i]));
		}

		// TODO
	}
};

}

#endif
