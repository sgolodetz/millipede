/***
 * millipede: SegmentationRun.h
 * Jess Pumprey, 2012
 ***/

#ifndef H_MILLIPEDE_DIFFERENCEOPTIONS
#define H_MILLIPEDE_DIFFERENCEOPTIONS

#include <sstream>

#include <common/jobs/Job.h>

#include <common/math/NumericUtil.h>
#include <mast/models/PartitionModel.h>
#include <mast/util/StringConversion.h>
#include <common/segmentation/DICOMLowestLayersBuilder.h>
#include <common/segmentation/VolumeIPFBuilder.h>
#include <iostream>

namespace mp { 


/*****
 * Options for finding the difference between two runs
 * 
 * 
 * 
 * 
 * 
 * 
 * ****/


class DifferenceOptions {
	
public:

	unsigned layerA, layerB;
	unsigned ipfA, ipfB;
	
	enum DifferenceType { MINUS = 0, UNION = 1, INTERSECTION = 2 };
	
	DifferenceType type;
	
	DifferenceOptions(unsigned lA, unsigned lB, unsigned iA, unsigned iB, DifferenceType t) {
		layerA = lA;
		layerB = lB;
		ipfA = iA;
		ipfB = iB;
		type = t;
	}
	
	DifferenceOptions() {};
	
	
private:
	
	
	
	
};

}
  


#endif