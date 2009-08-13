#ifndef MYWATERSHEDSEGMENTER_H_
#define MYWATERSHEDSEGMENTER_H_

#include "itkImage.h"
#include "DisjointSetForest.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkConstantBoundaryCondition.h"
#include <climits>
#include <queue>
#include <vector>
#include <map>
#include <iostream>
#include <utility>
using namespace itk;


template<class TInputImage>
class ITK_EXPORT MyWatershedSegmenter
{
	
private:
	struct compare {
		bool operator()(typename TInputImage::IndexType index1, typename TInputImage::IndexType index2){
			if(index1[0] < index2[0]) {
				return true;
			} else if ((index1[0] == index2[0]) && (index1[1] < index2[1])) {
				return true;
			} else {
				return false;
			}
		}
	};
	TInputImage* initialImage;
	typename TInputImage::Pointer lowerCompleteImage;
	typename TInputImage::Pointer labeledImage;

	typedef typename TInputImage::IndexType pixelCoord;
	std::map<pixelCoord, pixelCoord, compare> arrows;

	typedef typename ConstNeighborhoodIterator<TInputImage>::RadiusType radiusType;
	radiusType radius;

	//std::vector<Node<typename TInputImage::IndexType> > nodeList;
	typename TInputImage::IndexType marker;
	ConstNeighborhoodIterator<TInputImage> iterator;
	int dimension;
	int neighborhoodSize;
	int centerPixelIndex;

	
	ImageRegionIterator<TInputImage> initialImageIterator;
	ImageRegionIterator<TInputImage> lowerCompleteImageIterator;
	ImageRegionIterator<TInputImage> labeledImageIterator;
	
	NeighborhoodIterator<TInputImage> initialImageNeighIterator;
	NeighborhoodIterator<TInputImage> lowerCompleteImageNeighIterator;
	NeighborhoodIterator<TInputImage> labeledImageNeighIterator;
	ConstantBoundaryCondition<TInputImage> condition;
		
		

	
public:
	/*Constructor: assignes memory for the lower complete image and final label image 
	 * and creates the iterators
	 * 
	 * @param the image the watershed is performed on*/
	MyWatershedSegmenter(TInputImage *image);
	
	
	virtual ~MyWatershedSegmenter();
	
	/*Removes all the non-minimal plateaus in the image
	 * 
	 * @return The lower complete image*/
	void buildLowerCompleteImage();
	
	/*Returns the final labeled image
	 * 
	 * @return The labeled image*/
	void buildLabeledImage();
	TInputImage *returnFinalImage();
	TInputImage *returnLCImage();

private:
	/*Assignes all pixels in the image to a local minimum*/
	void resolveAll();
	
	/* Returns the local minimum that this pixel
	 * should be assigned to
	 * 
	 * @param index - the pixel we want to assign to a minimum
	 * @return - the pixel coordinates of the local minimum for the
	 * given pixel*/
	pixelCoord resolvePixel(pixelCoord index);
};

#include "MyWatershedSegmenter.tpp"

#endif /*MYWATERSHEDSEGMENTER_H_*/

