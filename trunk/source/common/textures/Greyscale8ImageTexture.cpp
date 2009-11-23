/***
 * millipede: Greyscale8ImageTexture.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Greyscale8ImageTexture.h"

#include <GL/glu.h>

#include <itkPasteImageFilter.h>

namespace mp {

//#################### CONSTRUCTORS ####################
Greyscale8ImageTexture::Greyscale8ImageTexture(const ImagePointer& image, bool clamp)
:	Texture(clamp), m_image(image)
{
	reload();
}

//#################### PROTECTED METHODS ####################
void Greyscale8ImageTexture::reload_image() const
{
	typedef itk::PasteImageFilter<Image,Image> Paster;

	Image::SizeType size = m_image->GetLargestPossibleRegion().GetSize();

	// Pad the image so that its dimensions are powers of two if necessary.
	int desiredWidth = 1, desiredHeight = 1;
	while(desiredWidth < size[0]) desiredWidth *= 2;
	while(desiredHeight < size[1]) desiredHeight *= 2;
	if(desiredWidth != size[0] || desiredHeight != size[1])
	{
		Paster::Pointer paster = Paster::New();

		Image::IndexType index;
		index.Fill(0);

		Image::Pointer paddedImage = Image::New();
		Image::RegionType paddedRegion;
		Image::SizeType paddedSize;
		paddedSize[0] = desiredWidth;
		paddedSize[1] = desiredHeight;
		paddedRegion.SetIndex(index);
		paddedRegion.SetSize(paddedSize);
		paddedImage->SetRegions(paddedRegion);
		paddedImage->Allocate();
		paddedImage->FillBuffer(0);

		paster->SetSourceImage(m_image);
		paster->SetSourceRegion(m_image->GetLargestPossibleRegion());
		paster->SetDestinationImage(paddedImage);
		paster->SetDestinationIndex(index);
		paster->Update();

		gluBuild2DMipmaps(GL_TEXTURE_2D, 1, desiredWidth, desiredHeight, GL_LUMINANCE, GL_UNSIGNED_BYTE, paster->GetOutput()->GetBufferPointer());
	}
	else
	{
		gluBuild2DMipmaps(GL_TEXTURE_2D, 1, size[0], size[1], GL_LUMINANCE, GL_UNSIGNED_BYTE, m_image->GetBufferPointer());
	}
}

}
