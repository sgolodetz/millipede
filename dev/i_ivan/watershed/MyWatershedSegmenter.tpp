template<class TInputImage> MyWatershedSegmenter<TInputImage>::MyWatershedSegmenter(
		TInputImage *image) {

	initialImage = image;

	radius.Fill(1);

	lowerCompleteImage= TInputImage::New();
	lowerCompleteImage->SetRegions(initialImage->GetLargestPossibleRegion());
	lowerCompleteImage->Allocate();

	labeledImage = TInputImage::New();
	labeledImage->SetRegions(initialImage->GetLargestPossibleRegion());
	labeledImage->Allocate();

	dimension = initialImage->GetImageDimension();
	neighborhoodSize = (int) pow(3, dimension);
	centerPixelIndex = ((int) pow(3, dimension)) / 2;
	for (int i = 0; i < dimension; i++) {
		marker[i] = -1;
	}
	
	//initialize image iterators
	initialImageIterator = ImageRegionIterator<TInputImage> (initialImage,
			initialImage->GetLargestPossibleRegion());
	lowerCompleteImageIterator = ImageRegionIterator<TInputImage> (lowerCompleteImage, lowerCompleteImage->GetLargestPossibleRegion());
	labeledImageIterator = ImageRegionIterator<TInputImage> (labeledImage, labeledImage->GetLargestPossibleRegion());
	
	//initialize neighborhood iterators
	condition.SetConstant(INT_MAX);
	initialImageNeighIterator = NeighborhoodIterator<TInputImage> (radius, initialImage,
			initialImage->GetLargestPossibleRegion());
	lowerCompleteImageNeighIterator = NeighborhoodIterator<TInputImage> (radius, 			lowerCompleteImage, lowerCompleteImage->GetLargestPossibleRegion());
	labeledImageNeighIterator = NeighborhoodIterator<TInputImage> (radius, labeledImage, 		labeledImage->GetLargestPossibleRegion());
	initialImageNeighIterator.OverrideBoundaryCondition(&condition);
	lowerCompleteImageNeighIterator.OverrideBoundaryCondition(&condition);
	labeledImageNeighIterator.OverrideBoundaryCondition(&condition);

	
	initialImageIterator = ImageRegionIterator<TInputImage> (initialImage,
			initialImage->GetLargestPossibleRegion());
	lowerCompleteImageIterator = ImageRegionIterator<TInputImage> (lowerCompleteImage, 			lowerCompleteImage->GetLargestPossibleRegion());
	labeledImageIterator = ImageRegionIterator<TInputImage> (labeledImage, 					labeledImage->GetLargestPossibleRegion());
}

template<class TInputImage> MyWatershedSegmenter<TInputImage>::~MyWatershedSegmenter() {

}

template<class TInputImage> void MyWatershedSegmenter<TInputImage>::buildLowerCompleteImage() {
	std::queue<typename TInputImage::IndexType> pixelQueue;
	int dist = 1;

	for (initialImageNeighIterator.GoToBegin(), lowerCompleteImageNeighIterator.GoToBegin(); 		!initialImageNeighIterator.IsAtEnd(); ++initialImageNeighIterator, 			++lowerCompleteImageNeighIterator) {
		lowerCompleteImageNeighIterator.SetCenterPixel(0);
		//check if the pixel has a lower neighbor
		//for all pixels in the neighborhood
		for (int i =0; i < neighborhoodSize; i++) {
			//other than the center pixel
			if (i != centerPixelIndex) {
				if (initialImageNeighIterator.GetPixel(i) < initialImageNeighIterator.GetCenterPixel()) {
					//add all non-minimum pixels to the queue
					pixelQueue.push(initialImageNeighIterator.GetIndex());
					lowerCompleteImageNeighIterator.SetCenterPixel(-1);
					break;
				}
			}
		}
	}

	pixelQueue.push(marker);
	while (!pixelQueue.empty()) {
		typename TInputImage::IndexType currentPixel = pixelQueue.front();
		pixelQueue.pop();
		if (currentPixel == marker) {
			if (!pixelQueue.empty()) {
				//we've gone through all pixels within n of the plateau edge
				dist++;
				pixelQueue.push(marker);
			}
		} else {
			initialImageNeighIterator.SetLocation(currentPixel);
			lowerCompleteImageNeighIterator.SetLocation(currentPixel);
			lowerCompleteImageNeighIterator.SetCenterPixel(dist);
			for (int i =0; i < neighborhoodSize; i++) {
				if (i != centerPixelIndex) {
					//a neighboring pixel is of the same height and hasn't been processed yet
					if (initialImageNeighIterator.GetPixel(i)== initialImageNeighIterator.GetCenterPixel()
							&& lowerCompleteImageNeighIterator.GetPixel(i) == 0) {
						pixelQueue.push(initialImageNeighIterator.GetIndex(i));
						lowerCompleteImageNeighIterator.SetPixel(i, -1);
					}
				}
			}
		}
	}

	for (initialImageIterator.GoToBegin(), lowerCompleteImageIterator.GoToBegin(); !initialImageIterator.IsAtEnd(),
			!lowerCompleteImageIterator.IsAtEnd(); ++lowerCompleteImageIterator, ++initialImageIterator) {
		if (lowerCompleteImageIterator.Get() != 0) {
			lowerCompleteImageIterator.Set(initialImageIterator.Get() * dist
					+ lowerCompleteImageIterator.Get() - 1);
		}
	}

}

template<class TInputImage> void MyWatershedSegmenter<TInputImage>::buildLabeledImage() {
	cp::DisjointSetForest<typename TInputImage::IndexType> minima;
	int labelCount = 0;
	
	for (lowerCompleteImageIterator.GoToBegin(), labeledImageIterator.GoToBegin(); !lowerCompleteImageIterator.IsAtEnd(),
			!labeledImageIterator.IsAtEnd(); ++lowerCompleteImageIterator, ++labeledImageIterator) {
		//label each minimum pixel as a different region
		if (lowerCompleteImageIterator.Get() == 0) {
			labeledImageIterator.Set(labelCount);
			labelCount ++;
			typename TInputImage::IndexType index = lowerCompleteImageIterator.GetIndex();
			minima.add_node(index);
		}
	}

	for (lowerCompleteImageNeighIterator.GoToBegin(), labeledImageNeighIterator.GoToBegin(); !lowerCompleteImageNeighIterator.IsAtEnd()
			&& !labeledImageNeighIterator.IsAtEnd(); ++lowerCompleteImageNeighIterator,
			++labeledImageNeighIterator) {
		//union all neghboring minimum points
		if (lowerCompleteImageNeighIterator.GetCenterPixel() == 0) {
			for (int i = 0;  i < neighborhoodSize; i++) {
				if (i != centerPixelIndex) {
					if (lowerCompleteImageNeighIterator.GetPixel(i) == 0) {
						minima.union_nodes(labeledImageNeighIterator.GetCenterPixel(),labeledImageNeighIterator.GetPixel(i));
					}

				}
			}
		} else { //looks for minimum neighbour
			typename TInputImage::IndexType lowestNeighbor = marker;
			int lowestNeighborValue = INT_MAX;
			for (int i = 0; i < neighborhoodSize; i++) {
				if (i != centerPixelIndex) {
					//std::cout << i << ' ' << (pow(3, dimension) / 2) << '\n';
					if (lowerCompleteImageNeighIterator.GetPixel(i) < lowestNeighborValue) {
						//std::cout << 2;
						lowestNeighbor = lowerCompleteImageNeighIterator.GetIndex(i);
						lowestNeighborValue = lowerCompleteImageNeighIterator.GetPixel(i);
					}

				}
			}
			//std::cout <<labelNeighIterator.GetIndex() << " --> "
			//		<< lowestNeighbor << "\n";
			arrows[labeledImageNeighIterator.GetIndex()] = lowestNeighbor;
		}

	}

	labelCount = 1;
	for (lowerCompleteImageIterator.GoToBegin(), labeledImageIterator.GoToBegin(); !lowerCompleteImageIterator.IsAtEnd(), !labeledImageIterator.IsAtEnd(); ++lowerCompleteImageIterator, ++labeledImageIterator) {
		if (lowerCompleteImageIterator.Get() != 0) {
			continue;
		}
		int labelValue = labeledImageIterator.Get();

		int root = minima.find_set(labelValue);
		if (root == labeledImageIterator.Get()) {
			arrows[labeledImageIterator.GetIndex()] = labeledImageIterator.GetIndex();
			labeledImageIterator.Set(labelCount++);
		} else {
			arrows[labeledImageIterator.GetIndex()] = minima.value_of(root);
		}
	}

	
	resolveAll();	
	
	for (labeledImageIterator.GoToBegin(); !labeledImageIterator.IsAtEnd(); ++labeledImageIterator) {
		
		labeledImageIterator.Set(labeledImage->GetPixel(arrows[labeledImageIterator.GetIndex()]));
		
		//This was producing a one-pixel region that was then passed to a node
		//and then it was creating a graph where each pixel was connected to all of its 
		//neighbors, with an edge of weight 0 if they had the same label and max(this pixel, neighbor)
		//othewise
		/*
		std::vector<typename TInputImage::IndexType> pixelIndeces;
		pixelIndeces.push_back(labeledImageIterator.GetIndex());
		MyImageRegion<typename TInputImage::IndexType> region(pixelIndeces);
		std::vector<std::pair<Node<typename TInputImage::IndexType> *, int > > neighborNodes;
		Node<typename TInputImage::IndexType> node(neighborNodes, region);
		nodeList.push_back(node);
		
	}

	for (labeledImageNeighIterator.GoToBegin(), lowerCompleteImageNeighIterator.GoToBegin(); !labeledImageNeighIterator.IsAtEnd() && !lowerCompleteImageNeighIterator.IsAtEnd(); ++labeledImageNeighIterator, ++lowerCompleteImageNeighIterator) {
		dimension = initialImage->GetImageDimension();
		int vectorIndex = 0;
		for (int j = 0; j < dimension - 1; j++) {
			vectorIndex += lowerCompleteImage->GetLargestPossibleRegion().GetSize()[j] *
			labeledImageNeighIterator.GetIndex()[j];
		}
		vectorIndex += labeledImageNeighIterator.GetIndex()[dimension - 1];

		for (int i =0; i < neighborhoodSize; i++) {
			if (i != centerPixelIndex) {
				int vectorNeighIndex = 0;
				
				for (int j = 0; j < dimension - 1; j++) {
					vectorNeighIndex += lowerCompleteImage->GetLargestPossibleRegion().GetSize()[j] *
					labeledImageNeighIterator.GetIndex(i)[j];
				}
				vectorNeighIndex += labeledImageNeighIterator.GetIndex(i)[dimension - 1];
				if (labeledImageNeighIterator.GetPixel(i) == labeledImageNeighIterator.GetCenterPixel()) {
					nodeList[vectorIndex].addNeighbor(&(nodeList[vectorNeighIndex]), 0);
				} else {
					nodeList[vectorIndex].addNeighbor(&(nodeList[vectorNeighIndex]), std::max(lowerCompleteImageNeighIterator.GetCenterPixel(), lowerCompleteImageNeighIterator.GetPixel(i)));
				}

			}
		}
	}*/
	
}
}

template<class TInputImage> void MyWatershedSegmenter<TInputImage>::resolveAll() {
	for (labeledImageIterator.GoToBegin(); !labeledImageIterator.IsAtEnd(); ++labeledImageIterator) {
		arrows[labeledImageIterator.GetIndex()] = resolvePixel(labeledImageIterator.GetIndex());
	}
}

template<class TInputImage> typename TInputImage::IndexType MyWatershedSegmenter<
		TInputImage>::resolvePixel(typename TInputImage::IndexType pixel) {
	typename TInputImage::IndexType parent = arrows[pixel];
	while (parent != pixel) {
		pixel = parent;
		parent = arrows[pixel];
	}
	return parent;
}

template<class TInputImage> TInputImage* MyWatershedSegmenter<TInputImage>::returnFinalImage() {
	return labeledImage;
}

template<class TInputImage> TInputImage* MyWatershedSegmenter<TInputImage>::returnLCImage() {
	return lowerCompleteImage;
}

/*template<class TInputImage> TInputImage* MyWatershedSegmenter<TInputImage>::returnNodeMST() {
	return lowerCompleteImage;
}*/




